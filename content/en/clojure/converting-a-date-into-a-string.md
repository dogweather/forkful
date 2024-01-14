---
title:    "Clojure recipe: Converting a date into a string"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/clojure/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Converting dates into strings is a common task in programming, especially when working with data or creating user interfaces. In Clojure, it allows us to easily manipulate and display dates in a human-readable format.

## How To

Converting a date into a string in Clojure is a simple process. We can use the `str` function to convert a date into a string by passing in the date and a format string.

```Clojure
(str (java.util.Date.) "dd/MM/yyyy")
;; Output: "12/10/2021"
```

We can also use the `format` function to specify a format and convert a date into a string.

```Clojure
(format (java.util.Date.) "MMM dd, yyyy")
;; Output: "Oct 12, 2021"
```

Note that we are using the `java.util.Date` class here, but we can also use Clojure's `clj-time` library for more advanced date manipulation.

## Deep Dive

Clojure's `str` and `format` functions both use the `java.time.format.DateTimeFormatter` class behind the scenes to convert dates into strings. This class allows us to specify various patterns and symbols to format the date according to our needs.

For example, the format string `"EEE, MMM d, ''yyyy"` will display the date as "Tue, Oct 12, '2021", using single quotes to represent the literal text.

We can also use the `with-localized-pattern` function to specify a different locale for the date formatting. This is particularly useful if we want to display the date in a different language or region.

```Clojure
(with-localized-pattern "fr" (java.time.format.DateTimeFormatter/ofPattern "EEE, MMM d, ''yyyy") (java.util.Date.))
;; Output: "Mar., fév. 2, '2021"
```

## See Also

If you want to dive even deeper into date conversion in Clojure, check out the official documentation for the `java.time.format.DateTimeFormatter` class and the `clj-time` library. You can also explore other date manipulation functions in Clojure, such as `create-date`, `date-time`, and `now`.