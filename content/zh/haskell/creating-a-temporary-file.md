---
title:                "Haskell: 创建临时文件"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/zh/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

\#\# 为什么

创建临时文件是在Haskell编程中一个常见的任务。它可以帮助我们暂时存储数据或者给我们使用临时的工作空间。临时文件通常被用于保存程序的中间结果或者临时数据，而不是长期存储。

\#\# 如何操作

前往创建临时文件的第一步是导入 **System.IO.Temp** 模块。它包含了创建及操作临时文件的函数。然后，我们可以使用 **withSystemTempFile** 函数来创建一个临时文件，在这个函数的参数列表中，我们需要传入一个路径及一个函数。

````Haskell
import System.IO.Temp (withSystemTempFile)

main = withSystemTempFile "temFile.txt" $ \path handle -> do
    putStrLn $ "临时文件创建成功: " ++ path -- 输出：临时文件创建成功: temFile.txt

    hPutStrLn handle "这些是临时写入的内容。" -- 写入到临时文件中

    hClose handle -- 关闭文件句柄，这个文件在程序运行结束后会自动删除。
````

在这个例子中，我们使用了 **withSystemTempFile** 函数来创建一个名为 "temFile.txt" 的临时文件，然后将一些文本写入到文件中。最后，我们关闭了文件句柄。在这个函数中，我们使用了一个匿名函数来作为参数，这个函数有两个参数，一个是临时文件的路径，一个是文件的句柄（用于操作文件）。

如果我们想要创建一个临时文件夹，可以使用 **withSystemTempDirectory** 函数来代替 **withSystemTempFile** 函数，用法和参数列表都是一样的。

````Haskell
import System.IO.Temp (withSystemTempDirectory)

main = withSystemTempDirectory "temDir" $ \path -> do
    putStrLn $ "临时文件夹创建成功: " ++ path -- 输出：临时文件夹创建成功: temDir

    -- 在这里可以进行一些操作

    removeDirectoryRecursive path -- 删除临时文件夹，这个文件夹在程序运行结束后会自动删除。
````

在这个例子中，我们使用了 **withSystemTempDirectory** 函数来创建一个名为 "temDir" 的临时文件夹，然后进行一些操作，最后删除了这个临时文件夹。

\#\# 深入了解

我们可以使用 **openTempFile** 函数来手动创建一个临时文件。这个函数接受两个参数，一个是路径，一个是文件名的前缀。它会返回一个文件路径和一个文件句柄。

````Haskell
import System.IO.Temp (openTempFile)

main = do
    (path, handle) <- openTempFile "." "myTempFile.txt"

    putStrLn $ "临时文件创建成功: " ++ path -- 输出：临时文件创建成功: ./myTempFile.txt

    hPutStrLn handle "这些是临时写入的内容。" -- 写入到临时文件中

    hClose handle -- 关闭文件句柄

    removeFile path -- 删除临时文件
````

在这个例子中，我们手动创建了一个名为 "myTempFile.txt" 的临时文件，在指定的路径下。同样地，我们也可以使用 **openTempDirectory** 函数来手动创建一个临时文件夹。

````Haskell
import System.IO.Temp (openTempDirectory)

main = do
    path <- openTempDirectory "."

    putStrLn $ "临时文件夹创建成功: " ++ path -- 输出：临时文件夹创建成功: ./myTempDir

    -- 在这里可以进行一些操作

    removeDirectoryRecursive path -- 删除临时文件夹
````

通过手动创建临时文件和临时文件夹，我们可以更灵活地控制它们的位置和命名，以及进行一些其他的操作。

\#\# 相