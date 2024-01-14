---
title:    "Ruby recipe: Using regular expressions"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Have you ever encountered a situation where you need to find and manipulate specific patterns within a text? Regular expressions, also known as regex, can be a powerful tool in your programming toolkit for solving such problems. With regex, you can search, replace, and extract strings with specific patterns, making it a valuable skill for any programmer to have.

## How To

To use regular expressions in Ruby, you first need to require the `regexp` library:

```Ruby
require 'regexp'
```

Next, you can either create a new regex object with the `Regexp.new` method or use the regex literal notation by enclosing your pattern within forward slashes. For example:

```Ruby
/program/ #this is a regex literal
Regexp.new("ruby") #this creates a new regex object
```

To apply a regex to a string, you can use the `=~` operator, which returns the first index where a match is found. You can also use the `match` method to get a match object with more details about the match. For example:

```Ruby
"This is a Ruby programming blog".match(/Ruby/) #returns a match object
"This is a Ruby programming blog" =~ /Ruby/ #returns 10
```

You can also use special characters and modifiers to make your regular expressions more robust. Some commonly used characters and modifiers include:

- `.` matches any single character
- `*` matches the preceding character or group zero or more times
- `+` matches the preceding character or group one or more times
- `?` matches the preceding character or group zero or one time
- `^` matches the start of a string
- `$` matches the end of a string
- `i` modifier makes the regex case insensitive
- `m` modifier allows `^` and `$` to match the start and end of a line instead of the whole string

You can test and experiment with regular expressions using online regex tools like [Rubular](https://rubular.com/).

## Deep Dive

Regular expressions can get quite complex and advanced, but some additional features and syntax to note include:

- Character classes: enclosed in square brackets `[]`, these match any one character within the brackets. For example, `[aeiou]` will match any vowel.
- Alternation: using the `|` operator, you can match multiple patterns, similar to a logical OR. For example, `cat|dog` will match either "cat" or "dog".
- Capturing groups: using parentheses `()`, you can define subpatterns within a larger regex and extract these specific matches later. For example, `/(Ruby) programming/` will capture "Ruby" in a match object.
- Quantifiers: modifiers that specify the number of times a character or group should be matched. For example, `{3}` matches exactly 3 times, `{3,}` matches 3 or more times, and `{3,8}` matches 3 to 8 times.
- Lookaround: these specify boundaries for a match but do not include the characters in the final match. For example, `(?=Ruby)` matches "Ruby programming" but only includes "Ruby" in the final match.

To learn more about regular expressions, you can refer to the [Ruby documentation](https://ruby-doc.org/core-3.0.1/Regexp.html) and other online tutorials and resources.

See Also

- [Rubular](https://rubular.com/)
- [RegexOne](https://regexone.com/)
- [Ruby Regex Guide](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Regular-Expressions.info](https://www.regular-expressions.info/)