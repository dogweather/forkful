---
title:    "C# recipe: Converting a string to lower case"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why
Converting a string to lower case is a common task in C# programming, especially when working with user input or comparing strings. By converting a string to lower case, you can ensure that the string is in a standardized format, making it easier to manipulate and compare with other strings. This also allows for better user experience, as users may not always type in the exact uppercase or lowercase letters.

## How To
To convert a string to lower case in C#, you can use the `ToLower()` method. This method is available on all string objects and will return a new string with all characters converted to lower case. Let's take a look at an example:

```C#
string name = "John Smith";
string lowerCaseName = name.ToLower();

Console.WriteLine(lowerCaseName);
```

The above code will output `john smith`, as the `ToLower()` method has converted all characters in the `name` string to lower case. 

You can also use this method when comparing strings, to ensure that the comparison is not case sensitive. For example:

```C#
string userInput = Console.ReadLine();
string secretWord = "password";

if(userInput.ToLower() == secretWord){
    Console.WriteLine("Access granted!");
} else {
    Console.WriteLine("Incorrect password, try again.");
}
```

In the above code, the `userInput` is converted to lower case before comparing it with the `secretWord`, ensuring that the entered password is not case sensitive.

## Deep Dive
Behind the scenes, the `ToLower()` method uses the `ToLowerInvariant()` method, which takes into account culture-specific casing rules. This ensures that the conversion is consistent across different cultures and languages. 

It is also important to note that the `ToLower()` method returns a new string object rather than modifying the original string. This is because strings in C# are immutable, meaning they cannot be changed once created. 

Additionally, it is worth mentioning that C# also has a `ToUpper()` method, which converts a string to upper case using the same principles as the `ToLower()` method.

## See Also
- `ToLower()` Method (Microsoft Docs): https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower
- Immutable Objects in C# (Medium): https://medium.com/whoknowzwhilecode/immutable-objects-in-c-1f36da547f3a