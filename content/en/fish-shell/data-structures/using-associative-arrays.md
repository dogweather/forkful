---
date: 2024-01-30 18:57:10.590524-07:00
description: "How to: Fish doesn't natively support associative arrays like Bash 4+,\
  \ but you can achieve similar functionality using a combo of lists and string\u2026"
lastmod: '2024-03-13T22:45:00.465827-06:00'
model: gpt-4-0125-preview
summary: Fish doesn't natively support associative arrays like Bash 4+, but you can
  achieve similar functionality using a combo of lists and string manipulation.
title: Using associative arrays
weight: 15
---

## How to:
Fish doesn't natively support associative arrays like Bash 4+, but you can achieve similar functionality using a combo of lists and string manipulation. Here’s how to mimic them:

First, setting up "associative array" elements separately:

```Fish Shell
set food_color_apple "red"
set food_color_banana "yellow"
```

To access an element, just reference it directly:

```Fish Shell
echo $food_color_apple
# Output: red
```

If you need to iterate over them, use a for-loop considering a naming convention:

```Fish Shell
for food in apple banana
    echo $food_color_$food
end
# Output:
# red
# yellow
```

For those missing Bash's `${!array[@]}` to get all keys, you can store keys in a separate list:

```Fish Shell
set food_keys apple banana

for key in $food_keys
    echo $key 'is' $food_color_$key
end
# Output:
# apple is red
# banana is yellow
```

## Deep Dive
Truly associative arrays as in other scripting languages aren't yet a part of Fish's approach. The workaround shown leverages Fish's string manipulation and list capabilities to create a pseudo-associative array structure. While it works, it's not as clean or error-proof as built-in associative array support would be. Other shells like Bash and Zsh provide built-in associative array functionality, which results in more straightforward, readable code. However, Fish's design philosophy aims for simplicity and user-friendliness, possibly at the expense of such features. The workaround satisfies most needs but keep an eye on the Fish Shell's evolution—its developers actively improve and add features based on community feedback.
