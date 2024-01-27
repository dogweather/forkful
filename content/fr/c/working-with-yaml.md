---
title:                "Travailler avec YAML"
date:                  2024-01-19
html_title:           "Bash: Travailler avec YAML"
simple_title:         "Travailler avec YAML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/c/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML, "YAML Ain't Markup Language", c'est un format de données lisible par l'humain pour configurer des projets. Les devs l'aiment pour sa simplicité et sa facilité d'emploi dans l'automatisation et la configuration.

## How to:
Tu ne peux pas manipuler YAML directement en C sans librairie externe. On va utiliser `libyaml` :

1. **Read YAML**

   ```C
   #include <yaml.h>

   int main() {
       FILE *fh = fopen("config.yaml", "r");
       yaml_parser_t parser;
       yaml_event_t event;

       yaml_parser_initialize(&parser);
       yaml_parser_set_input_file(&parser, fh);

       while (true) {
           if (!yaml_parser_parse(&parser, &event)) break;

           if (event.type == YAML_SCALAR_EVENT) {
               printf("Value: %s\n", event.data.scalar.value);
           }

           if (event.type != YAML_STREAM_END_EVENT) {
               yaml_event_delete(&event);
           } else {
               break;
           }
       }

       yaml_parser_delete(&parser);
       fclose(fh);

       return 0;
   }
   ```
   Output:
   ```
   Value: example_value
   ```

2. **Write YAML**

   ```C
   #include <yaml.h>

   int main() {
       FILE *fh = fopen("config.yaml", "w");
       yaml_emitter_t emitter;
       yaml_event_t event;

       yaml_emitter_initialize(&emitter);
       yaml_emitter_set_output_file(&emitter, fh);

       yaml_stream_start_event_initialize(&event, YAML_UTF8_ENCODING);
       yaml_emitter_emit(&emitter, &event);

       yaml_document_start_event_initialize(&event, NULL, NULL, NULL, 0);
       yaml_emitter_emit(&emitter, &event);

       yaml_scalar_event_initialize(&event, NULL, NULL,
           (unsigned char *)"example_key", -1, 1, 1, YAML_PLAIN_SCALAR_STYLE);
       yaml_emitter_emit(&emitter, &event);

       yaml_scalar_event_initialize(&event, NULL, NULL,
           (unsigned char *)"example_value", -1, 1, 1, YAML_PLAIN_SCALAR_STYLE);
       yaml_emitter_emit(&emitter, &event);

       yaml_document_end_event_initialize(&event, 0);
       yaml_emitter_emit(&emitter, &event);

       yaml_stream_end_event_initialize(&event);
       yaml_emitter_emit(&emitter, &event);

       yaml_emitter_delete(&emitter);
       fclose(fh);

       return 0;
   }
   ```

   Sur la sortie, tu trouveras un fichier `config.yaml` avec le contenu :
   ```
   example_key: example_value
   ```

## Deep Dive
YAML est né au début des années 2000, il est pratique et maintenant très commun. JSON et XML sont des alternatives, plus verbeuses. YAML est utilisé en C via des librairies comme `libyaml`, qui fait le parsing et l'émission de fichiers YAML. C'est bas niveau, donc parfois tu voudras peut-être une librairie qui mappe YAML sur des structures C directement.

## See Also
- Spec YAML: http://yaml.org/spec/1.2/spec.html
- LibYAML GitHub: https://github.com/yaml/libyaml
- YAML tutorial: https://learnxinyminutes.com/docs/yaml/
