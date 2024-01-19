---
title:                "Interpolating a string"
html_title:           "Arduino recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

# An Informal Take on String Interpolation in Go

## What & Why?

String interpolation is about inserting variables into a string. It's a handy trick when you need to whip up some message, log or output that's tailor-made for the situation at hand.

## How to:

Here's how it works in Go. Let's start by declaring a couple of variables:

```Go
name := "Gopher"
age := 10
```

We can insert these into a string using `fmt.Sprintf`:

```Go
message := fmt.Sprintf("Hello, my name is %s and I am %d years old.", name, age)
fmt.Println(message)
```

This prints: 

```Go
Hello, my name is Gopher and I am 10 years old.
```

Neat, right?

## Deep Dive

Go doesn't actually have built-in string interpolation like some languages (we're looking at you, Python and Ruby). But Go's `fmt` package gives us all we need with `Sprintf`, which is about as old-school as you get.

There are alternatives. You could concatenate strings with `+`, but this would be sluggish for large strings. Or you could use the `strings.Builder` type to concatenate more efficiently, but this can be an overkill for smaller strings.

On closer inspection, `Sprintf` works by parsing the format string, locating the `%` verbs and replacing them with the provided values. It's a nifty piece of technical plumbing that's been around since the C language's `printf` function.

## See Also

For more info, check out the official Go documentation for the [`fmt` package](https://golang.org/pkg/fmt/) and the [Effective Go guide](https://golang.org/doc/effective_go.html) on string formatting. Don't shy away from diving deep into Go's syntax. It's an adventure that's sure to turn you into a more effective programmer.