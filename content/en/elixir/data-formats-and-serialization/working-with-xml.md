---
date: 2024-01-25 03:39:47.117770-07:00
description: "Working with XML in Elixir means parsing, creating, and manipulating\
  \ XML data. Programmers tackle XML because it's widespread in web services, config\u2026"
lastmod: '2024-03-11T00:14:33.661144-06:00'
model: gpt-4-1106-preview
summary: "Working with XML in Elixir means parsing, creating, and manipulating XML\
  \ data. Programmers tackle XML because it's widespread in web services, config\u2026"
title: Working with XML
---

{{< edit_this_page >}}

## What & Why?
Working with XML in Elixir means parsing, creating, and manipulating XML data. Programmers tackle XML because it's widespread in web services, config files, and legacy systems.

## How to:
Elixir doesn't include XML parsing in its standard library. SweetXML is a popular choice. Here's how to use it:

```elixir
# Add SweetXML to your dependencies in mix.exs
{:sweet_xml, "~> 0.6"}

# In your code
import SweetXml

xml = """
<note>
  <to>Tove</to>
  <from>Jani</from>
  <heading>Reminder</heading>
  <body>Don't forget me this weekend!</body>
</note>
"""

# Parse XML
note = xml |> xpath(~x"//note")
to = xml |> xpath(~x"//note/to" |> inner_text())
IO.puts to # Output: Tove
```

## Deep Dive
XML, or Extensible Markup Language, has been around since the late 90s. It's verbose but structured—ideal for complex data interchange. While JSON's popularity soared for its simplicity, XML remains entrenched in many enterprise and financial systems for its expressiveness and standardized schemas.

Alternatives include:
- JSON for lighter, less verbose data exchange.
- Protobuf or Thrift for binary serialized data communication, particularly for internal systems.

Under the hood, XML libraries for Elixir leverage Erlang's :xmerl library for parsing, which provides robust support but can be less intuitive than more modern approaches. As Elixir evolves, community-driven libraries like SweetXML wrap these with a more Elixir-esque syntax, making XML manipulations more accessible.

## See Also:
- SweetXML on Hex: https://hex.pm/packages/sweet_xml
- Elixir's take on XML parsing: https://elixir-lang.org/getting-started/mix-otp/dependencies-and-umbrella-projects.html
- xmerl documentation for underlying XML handling: http://erlang.org/doc/apps/xmerl/index.html
