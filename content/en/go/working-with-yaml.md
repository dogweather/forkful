---
title:                "Working with YAML"
date:                  2024-01-19
html_title:           "Arduino recipe: Working with YAML"
simple_title:         "Working with YAML"
programming_language: "Go"
category:             "Go"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
Working with YAML means parsing and generating data in the YAML format, a human-readable data serialization standard. Programmers do it to manage configuration files, data interchange between languages, and structure complex data.

## How to:
To work with YAML in Go, you'll need a library like `gopkg.in/yaml.v3`. Install it using:

```bash
go get gopkg.in/yaml.v3
```

Here's how to parse YAML:

```Go
package main

import (
	"fmt"
	"log"
	"gopkg.in/yaml.v3"
)

var data = `
a: Easy!
b:
  c: 2
  d: [3, 4]
`

type StructA struct {
	A string
	B StructB
}

type StructB struct {
	C int
	D []int
}

func main() {
	var s StructA

	err := yaml.Unmarshal([]byte(data), &s)
	if err != nil {
		log.Fatalf("error: %v", err)
	}
	fmt.Println(s)
}
```

Output:

```
{Easy! {2 [3 4]}}
```

Generating YAML:

```Go
package main

import (
	"fmt"
	"gopkg.in/yaml.v3"
)

func main() {
	data := StructA{
		A: "Easy!",
		B: StructB{
			C: 2,
			D: []int{3, 4},
		},
	}

	d, err := yaml.Marshal(&data)
	if err != nil {
		log.Fatalf("error: %v", err)
	}
	fmt.Printf("---\n%s\n", string(d))
}
```

Output:

```
---
a: Easy!
b:
  c: 2
  d:
  - 3
  - 4
```

## Deep Dive
YAML started in 2001, with the goal of being a human-friendly data interchange format. It's used as an alternative to JSON and XML because it's more readable and can represent complex data structures. Go doesn't have built-in support for YAML, hence third-party libraries like `gopkg.in/yaml.v3` are popular. The library wraps libyaml, a C YAML parser and emitter, for efficiency and compliance with YAML standards.

## See Also
- The YAML v3 package documentation: https://pkg.go.dev/gopkg.in/yaml.v3
- Official YAML website: https://yaml.org
- YAML specification: https://yaml.org/spec/1.2/spec.html
- JSON to YAML online converter: https://www.json2yaml.com/
