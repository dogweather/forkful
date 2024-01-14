---
title:    "Haskell recipe: Checking if a directory exists"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

##Why

Haskell is a functional programming language that offers many powerful and efficient tools for building applications. One important aspect of creating robust programs is the ability to check for the existence of certain files or directories. In this blog post, we will delve into the world of checking directory existence in Haskell and explore the reasons why it is an important practice for developers.

##How To

In Haskell, we can use the `doesDirectoryExist` function from the `System.Directory` module to check if a given directory exists or not. Here's an example of how it works:

```Haskell
import System.Directory

main :: IO ()
main = do
    let dirPath = "/home/user/documents"
    dirExists <- doesDirectoryExist dirPath
    if dirExists
        then putStrLn "Directory exists!"
        else putStrLn "Directory does not exist."
```

In this code, we import the `System.Directory` module and use the `doesDirectoryExist` function to check if the directory at the specified path exists. The function returns a boolean value, which we can use in an if-else statement to print out a message accordingly.

Let's see what the output would be for different scenarios:

- If the directory at the given path does exist, the output would be: `Directory exists!`
- If the directory does not exist, the output would be: `Directory does not exist.`

It's as simple as that! Now let's take a deep dive into why this is an important practice for developers.

##Deep Dive

Checking for the existence of a directory may seem like a trivial task, but it can save a lot of headaches later on in the development process. Here are some reasons why it is important:

- Error handling: By checking for the existence of a directory before performing any operations on it, we can prevent our program from throwing an error and crashing. This is especially important when working with user-provided input or when dealing with external resources.
- Efficiency: If our program requires certain directories to be present in order to function properly, we can perform the existence check at the beginning of the program and then carry on with our operations. This can save us from repeatedly checking for existence later on, thus improving our program's efficiency.
- Cross-platform compatibility: Different operating systems have different file and directory structures. By using the `System.Directory` module, we can ensure that our code works consistently across different platforms.

##See Also

- [Haskell documentation on `System.Directory`](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [A tutorial on error handling in Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/10_Error_Handling)
- [Haskell wiki on cross-platform compatibility](https://wiki.haskell.org/Cross-platform)

In conclusion, checking for directory existence in Haskell is a simple yet important practice for developers. It can help us handle errors, improve efficiency, and ensure cross-platform compatibility. So next time you're working on a Haskell project, don't forget to include an existence check for your directories!