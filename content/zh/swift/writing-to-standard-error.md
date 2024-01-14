---
title:                "Swift: 编写标准错误"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

##为什么要写标准错误

写标准错误是一种将错误信息打印到控制台的方法。通过将错误信息输出到标准错误，我们可以更轻松地诊断和调试代码中的错误。这也可以帮助我们更快地找出问题并进行修复，提高编程效率。

##如何实施

若要将错误信息输出到标准错误，我们需要使用Swift中的print函数，并将标准错误作为参数传递给该函数。

```
Swift.print("错误信息", to: &stderr)
```
当我们运行这段代码时，在控制台中就会打印出错误信息。以下是一个示例：

```
func divide(_ dividend: Int, by divisor: Int) {
    guard divisor != 0 else{
        Swift.print("除数不能为0", to: &stderr)
        return
    }
    let result = dividend / divisor
    Swift.print(result, to: &stderr)
}
divide(10, by: 0)
```

输出结果为："除数不能为0"

##深入了解

在Swift中，标准错误被表示为文件流，可以通过outFileHandle属性访问。我们也可以使用FileHandle类来创建一个新的文件流，然后再将错误信息输出到该文件流。

```
let errorFileHandle = FileHandle.standardError
let errorMessage = "错误信息".data(using: .utf8)
errorFileHandle.write(errorMessage!)
```

在编写和调试代码时，我们还可以使用print函数的其他参数来自定义输出格式，进一步提升可读性和调试效率。

##另请参阅

- [Swift中的print函数](https://www.swift.com/documentation/swiftio.html#method-series)
- [如何使用标准错误输出正确地调试代码](https://www.debugging.com/blog/standard-error-in-swift/)