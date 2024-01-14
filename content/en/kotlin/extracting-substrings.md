---
title:    "Kotlin recipe: Extracting substrings"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Have you ever come across a situation where you needed to extract a specific part of a string in your code? Maybe you wanted to get the first name from a full name or extract a certain keyword from a longer sentence. In Kotlin, this can be easily achieved by using substring functions. Substring functions allow you to extract a portion of a string based on its index or length, making string manipulation much easier. In this blog post, we will explore how to use substring functions in Kotlin and how they can be valuable in your programming journey.

## How To

To extract substrings, we can use the `substring()` function in Kotlin. This function takes two parameters - the starting index and the ending index of the substring we want to extract. Let's look at an example:

```Kotlin
val fullName = "Jane Smith"
val firstName = fullName.substring(0, 4)
println(firstName)
```

In this example, we declare a string called `fullName` and initialize it with a value of "Jane Smith". Then, we use the `substring()` function to extract the first four characters, starting from index 0. The output of this code would be "Jane", which is the first name in the full name string.

We can also use the `substring()` function to extract a substring based on its length instead of its index. Let's look at another example:

```Kotlin 
val sentiment = "I'm feeling happy today!"
val emotion = sentiment.substring(13)
println(emotion)
```

In this code, we declare a string called `sentiment` and initialize it with a value of "I'm feeling happy today!". By using the `substring()` function and passing in only one parameter, we tell Kotlin to extract everything after index 13, which is where the word "happy" starts. The output of this code would be "happy today!", the emotion being expressed in the sentiment string.

Additionally, we can also use the `substringAfter()` and `substringBefore()` functions in Kotlin to extract substrings after or before a specific character or string. Let's see an example of how this can be useful:

```Kotlin
val email = "jane.smith@example.com"
val username = email.substringBefore("@")
println(username)
```

In this code, we have an email string and we want to extract the username part before the "@" symbol. By using the `substringBefore()` function, we can easily do that and get the output of "jane.smith".

## Deep Dive

It is important to note that the `substring()` function returns a new string and does not modify the original string. This means we can use it without worrying about changing the value of the original string. However, if we want to modify the original string, we can use the `replace()` function, which takes in the substring to be replaced and the new substring to be inserted.

Another important aspect of substring extraction is handling edge cases such as out-of-bounds indices. In Kotlin, if the starting index is larger than the string length or the ending index is out of bounds, the `IndexOutOfBoundsException` is thrown. To avoid this, we can use the `lastIndex` property to get the last index of the string and use it as a reference.

## See Also

For more information on substring functions in Kotlin, you can check out the official documentation: 
- [Kotlin - Basic Types](https://kotlinlang.org/docs/basic-types.html#strings)
- [substring() function](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/substring.html)
- [substringBefore() function](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/substring-before.html)
- [substringAfter() function](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/substring-after.html)

I hope this blog post helped you understand how to extract substrings in Kotlin and how they can be useful in your programming projects. Happy coding!