---
title:                "Working with TOML"
date:                  2024-01-25T03:39:28.278121-07:00
model:                 gpt-4-1106-preview
simple_title:         "Working with TOML"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/working-with-toml.md"
---

{{< edit_this_page >}}

## What & Why?
TOML, short for Tom's Obvious, Minimal Language, is a data serialization format. Programmers dig it for its simplicity and readability; it's primo for config files, similar vibes to YAML but less cumbersome than JSON for a human.

## How to:
First up, install `toml-cli` to play with TOML in Bash. Handy for reading or editing TOML files on the fly.

```Bash
# Install toml-cli, our little helper for TOML tasks
pip install toml-cli

# Imagine you've got a TOML file, 'config.toml'
echo -e 'title = "TOML Demo"\n\n[owner]\nname = "Tom"\ndob = 1979-05-27T07:32:00Z' > config.toml

# Read a value
toml get config.toml owner.name
# Output: Tom

# Set a value
toml set config.toml 'owner.dob' '2000-01-01T00:00:00Z'
# Pro tip: Use quotes for keys with dots or funky chars!
```

## Deep Dive
Born from the dislike of JSON's hurdles for humans, TOML dropped around 2013. Tom Preston-Werner, GitHub's co-founder, wanted something super legible. YAML and INI were alternatives but TOML's like the best of both. 

Shebang, you've got nested data and arrays, minus YAML's foot guns and JSON's curly braces. TOML's now a go-to for config in Rust's Cargo, which speaks to its rise in the dev world. It's driven by a spec, keeping things tight and well-defined. You'll nab parsers in almost any language, making it widely adoptable.

## See Also
- Official TOML GitHub Repo: https://github.com/toml-lang/toml
- toml-cli on PyPI: https://pypi.org/project/toml-cli/
- Comparison of data-serialization formats: https://en.wikipedia.org/wiki/Comparison_of_data-serialization_formats