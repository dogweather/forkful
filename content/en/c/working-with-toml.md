---
title:                "Working with TOML"
date:                  2024-01-25T03:40:08.712397-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with TOML"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c/working-with-toml.md"
---

{{< edit_this_page >}}

## What & Why?
TOML is a data serialization language designed to be easy to read and write. Programmers use it for config files, simple data storage, and cross-language data exchange due to its clarity and human-friendliness.

## How to:
Let's parse a TOML config file in C using the "tomlc99" library. First, install the library. Then, create a `config.toml`:

```toml
title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
```

Now, parse it in C:

```c
#include <stdio.h>
#include "toml.h"

int main() {
    FILE* fp;
    char errbuf[200];

    if (0 == (fp = fopen("config.toml", "r"))) {
        printf("Error: cannot open config file\n");
        return 1;
    }
    
    toml_table_t* conf = toml_parse_file(fp, errbuf, sizeof(errbuf));
    fclose(fp);
    if (0 == conf) {
        printf("Error: %s\n", errbuf);
        return 1;
    }

    printf("Title: %s\n", toml_raw_in(conf, "title"));

    toml_table_t* owner = toml_table_in(conf, "owner");
    printf("Owner Name: %s\n", toml_raw_in(owner, "name"));

    toml_free(conf);
    return 0;
}
```
Sample output:
```
Title: "TOML Example"
Owner Name: "Tom Preston-Werner"
```

## Deep Dive
TOML, which stands for Tom's Obvious, Minimal Language, was created by Tom Preston-Werner in 2013. It serves as a simpler alternative to formats like XML and YAML, focusing on being more human-readable and writable. While JSON is another alternative, TOML retains a structure that's easier to parse visually by humans, which is one of the primary reasons for its adoption in configuration files.

In C, working with TOML involves choosing a parser library since the language doesn't support it natively. Libraries like "tomlc99" are C99 compliant and provide an API to decode TOML text. When considering performance, proper error handling, and memory management are crucial as C doesn't have built-in garbage collection.

## See Also:
1. TOML Spec: [https://toml.io/en/](https://toml.io/en/)
2. tomlc99 GitHub repo: [https://github.com/cktan/tomlc99](https://github.com/cktan/tomlc99)
3. Comparing Data Serialization Formats: [https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html](https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html)
