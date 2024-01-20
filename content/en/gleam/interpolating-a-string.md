---
title:                "Interpolating a string"
html_title:           "Arduino recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

# A Quick-Start Guide to String Interpolation in Gleam

## What & Why?

String interpolation is basically synthesizing new strings using existing values. It helps programmers create dynamic strings without breaking a sweat.

## How to:

In Gleam, the interpolation macro `{}` is not supported. You'll have to slice and dice strings. Here's an example:

```gleam
fn main() {
  let name = "Gleam"
  let message = "Hello, " ++ name ++ ", good to see you!"
  message
}
```
Output:
```cli
"Hello, Gleam, good to see you!"
```
Concatenation with `++` lets us mix in variables into strings - the manual way. 

## Deep Dive

String Interpolation has not always been around. The folks who coded using `Fortran` had to write lines of code for creating a simple `hello world` string. String interpolation lightens the workload, and it's got some serious popularity in languages like Ruby, JavaScript, Python, etc.

In the case of Gleam, the language doesn't support interpolation directly. It takes inspiration from Erlang, a much older language without this feature. 

There are other alternatives like concatenation using `++` as seen above. Also possible are templates in some languages – defining a string structure and replacing placeholders with actual values.

As for how Gleam implements string manipulation, it leverages the Beam VM's binary data type. This makes string manipulation efficient but some modern features (like interpolation) aren't directly available.

## See Also

Dig deeper into Gleam's string manipulation - [Check out Gleam's Official Docs](https://docs.gleam.run/tour/strings).

If you're considering alternatives or have glimpses of sadness because of the lack of spiffy interpolation in Gleam, you can check out languages like [Elixir](https://elixir-lang.org/). 

And just in case you want to geek out on the history – the Fortran story’s told by [IBM itself](https://www.ibm.com/ibm/history/exhibits/fortran/fortran.html).