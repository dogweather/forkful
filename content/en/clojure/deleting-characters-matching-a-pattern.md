---
title:    "Clojure recipe: Deleting characters matching a pattern"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why
Have you ever found yourself with a string of text that contains unwanted characters? Maybe you need to clean up user input or remove a specific formatting style. Whatever the reason may be, knowing how to delete characters matching a pattern can come in handy for a variety of situations.

## How To
To delete characters matching a pattern in Clojure, we can use the `clojure.string/replace` function. This function takes in a string, a regular expression pattern, and either a replacement string or a function to manipulate the matched substring.

Let's say we have a string that contains a mix of uppercase and lowercase letters and we want to remove all the lowercase letters. We can do so by using the `#"[a-z]"` regular expression pattern, which matches any lowercase letter. Our code would look like this:

```
(clojure.string/replace "HeLlOwOrLd" #"[a-z]" "")
```

The output of this would be `"HLOWOLD"`, with all lowercase letters removed.

We can also use a function to manipulate the matched substring. For example, if we wanted to replace every occurrence of the letter "e" with "3", we could use the following code:

```
(clojure.string/replace "Hello world" #"e" (fn [match] "3"))
```

The output of this would be `"H3llo world"`. Pretty cool, right?

## Deep Dive
Now let's dive a bit deeper into what's happening behind the scenes. In the first example, we used the empty string as the replacement, essentially deleting the matched characters. Similarly, we could also use the `#"[a-z]"` pattern to replace lowercase letters with an empty string, resulting in the same output.

In the second example, we used a function to manipulate the matched substring. This function takes in the matched substring and returns the replacement string. In this case, we simply returned the string "3" for every "e" that was found.

Keep in mind that the `replace` function creates a new string, leaving the original string untouched. This can be useful if you need to preserve the original string for later use.

## See Also
- Official Clojure documentation on `clojure.string/replace`: https://clojuredocs.org/clojure.string/replace
- Regular expressions tutorial: https://www.regular-expressions.info/tutorial.html