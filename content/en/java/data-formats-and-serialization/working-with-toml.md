---
date: 2024-01-25 03:39:31.741670-07:00
description: "TOML stands for Tom's Obvious, Minimal Language. It's a data serialization\
  \ format used for config files. Programmers use it because it's easy to read,\u2026"
lastmod: '2024-03-13T22:44:59.992689-06:00'
model: gpt-4-1106-preview
summary: "TOML stands for Tom's Obvious, Minimal Language. It's a data serialization\
  \ format used for config files. Programmers use it because it's easy to read,\u2026"
title: Working with TOML
---

{{< edit_this_page >}}

## What & Why?
TOML stands for Tom's Obvious, Minimal Language. It's a data serialization format used for config files. Programmers use it because it's easy to read, write, and maps nicely to a hash table.

## How to:
You'll need a TOML parsing library. I recommend `toml4j`. Add it to your project like this:

```java
// Add this to your build.gradle
dependencies {
    implementation 'com.moandjiezana.toml:toml4j:0.7.2'
}
```

Here's how you parse a TOML file:

```java
import com.moandjiezana.toml.Toml;

public class TomlExample {
    public static void main(String[] args) {
        Toml toml = new Toml().read("""
            [server]
            ip = "192.168.1.1"
            port = 80
            """);

        String ip = toml.getString("server.ip");
        Integer port = toml.getLong("server.port").intValue();
        
        System.out.println("Server IP: " + ip);
        System.out.println("Server Port: " + port);
    }
}
```

Sample output:

```
Server IP: 192.168.1.1
Server Port: 80
```

## Deep Dive
Developed by GitHub co-founder Tom Preston-Werner, TOML aimed at being simpler than XML and more specified than YAML. Its latest version 1.0.0, released in 2021, offers a stable set of features.

Alternatives like JSON or YAML are also popular. JSON is great for data interchange. YAML is more human-readable for complex configs. TOML's strength is its straightforwardness and its use in the Rust community.

As for implementation, when using TOML with Java, keep in mind that the parser you pick matters. Beyond `toml4j`, some go for `jackson-dataformat-toml`. They'll each have nuances, like error handling or parsing performance, so choose based on your project's needs.

## See Also
- TOML Specification: https://toml.io/en/
- `toml4j` GitHub: https://github.com/mwanji/toml4j
- `jackson-dataformat-toml`: https://github.com/FasterXML/jackson-dataformats-text/tree/main/toml
