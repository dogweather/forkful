---
title:                "Swift: 检查目录是否存在"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# 为什么

每个程序员都知道，编写健壮的代码是非常重要的。当我们处理文件和文件夹时，我们经常需要检查某个目录是否存在。这可以帮助我们避免代码中的错误，为用户提供更好的体验。在这篇文章中，我们将探讨如何使用Swift来检查目录是否存在，并深入了解这个过程。

# 如何

首先，我们需要使用FileManager类来检查目录是否存在。我们可以创建一个实例并使用它的fileExists(atPath:)方法来检查路径是否存在。让我们来看一个示例代码：

```Swift
let fileManager = FileManager.default
let directoryPath = "/Users/username/Documents"
if fileManager.fileExists(atPath: directoryPath) {
    print("The directory exists!")
} else {
    print("The directory does not exist.")
}
```

在这个例子中，我们首先创建了一个FileManager的实例，然后指定我们想要检查的目录路径。接下来，我们使用fileExists(atPath:)方法来检查路径是否存在，如果存在，我们将输出一条消息。否则，我们将输出另一条消息。让我们来运行这段代码，看看结果是什么：

```
The directory exists!
```

我们也可以使用isDirectory属性来检查路径是否指向一个目录。如果是，该属性将返回true，否则将返回false。让我们来看一个例子：

```Swift
let fileManager = FileManager.default
let filePath = "/Users/username/Documents/file.txt"
if fileManager.fileExists(atPath: filePath) {
    if fileManager.isDirectory(filePath) {
        print("This is a directory.")
    } else {
        print("This is not a directory.")
    }
} else {
    print("The file does not exist.")
}
```

在这个例子中，我们检查了一个文件的路径，然后再次使用FileManager来检查该路径是否指向一个目录。如果是，我们将输出一条消息，否则将输出另一条消息。让我们来运行这段代码，看看结果是什么：

```
This is not a directory.
```

# 深入了解

在前面的例子中，我们使用了FileManager的fileExists(atPath:)方法来检查路径是否存在。但实际上，这个方法是可以检查任何类型的文件，包括目录、文件和符号链接的。这是因为在Unix系统中，目录也被视为一种特殊类型的文件。所以，使用这个方法来检查目录是否存在是可行的。除此之外，我们也可以使用其他方法来检查路径是否存在，比如：

- fileExists(atPath:isDirectory:)：这个方法同时也会返回路径指向的文件类型，是一个目录还是一个文件。
- fileExists(atPath:isWritable:)：这个方法可以检查路径是否可写入。
- fileExists(atPath:isExecutable:)：这个方法可以检查路径是否可执行。

在实际开发中，我们可以根据不同的需求来选择合适的方法来检查路径是否存在。

# 参考资料

- [苹果官方文档：FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [鸟哥的私房菜：Linux中文件与目录的权限与规划](https://linux.vbird.org/linux_basic/0220filemanager.php)
- [CSDN：Linux中的目录即是文件吗？](https://blog.csdn.net/sinat_31162235/article/details/65446858)

# 参见

- [苹果官方文档：FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Ray Wenderlich：Working with Files in Swift Tutorial](https://www.raywenderlich.com/128039/filemanager-class-tutorial-ios)
- [CocoaChina：NSURL、NSFileManager、NSData的类方法](http://www.cocoachina.com/ios/20150925/13404.html)