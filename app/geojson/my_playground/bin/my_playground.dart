import 'dart:convert';
import 'dart:io';

import 'package:csv/csv.dart';
import 'package:geojson/geojson.dart';

void main(List<String> arguments) async {
  print('Hello world!');

  Directory current = Directory.current;
  print(current.path);

  final barrios = File("../../data/barrios_riesgo.csv");
  final data1 = await barrios.readAsString();
  List<List<dynamic>> bdBarrios = const CsvToListConverter().convert(data1);

  final geojson = File("../../data/Barrio_Vereda.json");
  final data = await geojson.readAsString();

  final decoded = json.decode(data) as Map<String, dynamic>;
  final feats = decoded['features'] as List<dynamic>;

  final desconocidos = List<dynamic>();
  final bajos = [];
  final medios = [];
  final altos = [];

  for (final dfeature in feats) {
    final feat = dfeature as Map<String, dynamic>;
    var properties = <String, dynamic>{};
    if (feat.containsKey("properties")) {
      properties = feat["properties"] as Map<String, dynamic>;
    }
    properties['RIESGO'] = bdBarrios
            .firstWhere(
              (e) =>
                  equalsIgnoreAsciiCase(e[0], properties["NOMBRE"]) &&
                  equalsIgnoreAsciiCase(e[1], properties["NOMBRE_COM"]),
              orElse: () => null,
            )
            ?.elementAt(6) ??
        'desconocido';
    properties['RIESGO_NUMERICO'] = bdBarrios
            .firstWhere(
              (e) =>
                  equalsIgnoreAsciiCase(e[0], properties["NOMBRE"]) &&
                  equalsIgnoreAsciiCase(e[1], properties["NOMBRE_COM"]),
              orElse: () => null,
            )
            ?.elementAt(5) ??
        0;
    switch (properties['RIESGO']) {
      case 'desconocido':
        {
          desconocidos.add(feat);
        }
        break;
      case 'Bajo':
        {
          bajos.add(feat);
        }
        break;
      case 'Medio':
        {
          medios.add(feat);
        }
        break;
      case 'Alto':
        {
          altos.add(feat);
        }
        break;
    }
  }
  decoded['features'] = desconocidos;
  final jsonDesconocidos = File("desconocidos.json");
  jsonDesconocidos.writeAsStringSync(json.encode(decoded));

  decoded['features'] = bajos;
  final jsonBajos = File("bajos.json");
  jsonBajos.writeAsStringSync(json.encode(decoded));

  decoded['features'] = medios;
  final jsonMedios = File("medios.json");
  jsonMedios.writeAsStringSync(json.encode(decoded));

  decoded['features'] = altos;
  final jsonAltos = File("altos.json");
  jsonAltos.writeAsStringSync(json.encode(decoded));
}

const int _zero = 0x30;
const int _upperCaseA = 0x41;
const int _upperCaseZ = 0x5a;
const int _lowerCaseA = 0x61;
const int _lowerCaseZ = 0x7a;
const int _asciiCaseBit = 0x20;
bool equalsIgnoreAsciiCase(String a, String b) {
  if (a.length != b.length) return false;
  for (var i = 0; i < a.length; i++) {
    var aChar = a.codeUnitAt(i);
    var bChar = b.codeUnitAt(i);
    if (aChar == bChar) continue;
    // Quick-check for whether this may be different cases of the same letter.
    if (aChar ^ bChar != _asciiCaseBit) return false;
    // If it's possible, then check if either character is actually an ASCII
    // letter.
    var aCharLowerCase = aChar | _asciiCaseBit;
    if (_lowerCaseA <= aCharLowerCase && aCharLowerCase <= _lowerCaseZ) {
      continue;
    }
    return false;
  }
  return true;
}
