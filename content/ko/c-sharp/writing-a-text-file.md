---
title:                "C#: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

# 왜

텍스트 파일을 작성하는것에 참여하는 이유는 무엇일까요?

일반적으로 프로그램을 개발할 때, 우리는 데이터를 저장하고 유지하기 위해 다양한 파일 형식을 사용합니다. 이때, 텍스트 파일은 가장 많이 사용되는 형식 중 하나입니다. 텍스트 파일은 단순하게 문자의 나열로 구성되어 있기 때문에, 우리가 사용하는 다른 프로그램에서도 쉽게 열어서 데이터를 읽고 쓸 수 있습니다.

## 하우 투

그렇다면, 텍스트 파일을 어떻게 작성할 수 있을까요? 바로 C# 프로그래밍 언어를 사용하면 됩니다. 아래는 텍스트 파일을 작성하는 예제 코드와 결과물입니다.

```C#
// 텍스트 파일을 쓰기 위해 System.IO 네임스페이스를 추가합니다.
using System.IO;

// 파일 경로와 파일명을 지정합니다.
string path = @"C:\Users\Username\Documents\myTextFile.txt";

// 파일 쓰기를 위해 StreamWriter 객체를 생성합니다.
StreamWriter writer = new StreamWriter(path);

// 파일에 쓸 데이터를 지정합니다.
string data = "Hello, world!";

// Write 메소드를 사용하여 데이터를 파일에 씁니다.
writer.Write(data);

// 파일을 닫습니다.
writer.Close();

// 작성된 파일을 열어서 결과를 확인해봅니다.
// 파일 내용: Hello, world!
```

위 예제 코드에서는 `StreamWriter` 클래스를 사용하여 텍스트 파일을 작성하고 있습니다. 먼저 파일 경로와 파일명을 지정한 뒤, `StreamWriter` 객체를 생성하고 파일에 쓸 데이터를 지정한 뒤 `Write` 메소드를 사용하여 데이터를 파일에 씁니다. 마지막으로 `StreamWriter` 객체를 닫아줍니다. 이제 작성된 텍스트 파일을 열어보면 데이터가 정상적으로 저장되었는지 확인할 수 있습니다.

## 딥 다이브

지금까지는 간단한 텍스트 파일을 작성하는 예제를 살펴보았습니다. 하지만 더 깊이 들어가서 텍스트 파일을 다루는 다양한 기능들을 알아보는 것도 중요합니다.

먼저, `StreamWriter` 클래스의 생성자에는 여러 가지 옵션을 지정할 수 있는 인자들이 있습니다. 예를 들어, 파일을 덮어쓸지 추가할 지 여부를 지정하는 `append` 인자가 있습니다. 이를 사용하면 기존에 작성된 내용을 유지하면서 새로운 데이터를 추가할 수도 있습니다.

또한, `StreamWriter` 클래스의 `WriteLine` 메소드를 사용하면 문자열 뿐만 아니라 정수, 실수와 같은 다양한 데이터 형식도 파일에 쓸 수 있습니다. 이를 활용하여 프로그램에서 필요한 데이터를 파일에 저장할 수 있습니다.

마지막으로, `StreamWriter` 클래스의 `Flush` 메소드를 사용하면 버퍼에 저장된 데이터를 강제로 파일에 쓸 수 있습니다. 이를 사용하면 데이터를 쓰는 도중에 프로그램이 종료되거나 오류가 발생하여 파일에 모든 데이터가 올바르게 저장되지 않는 일을 방지할 수 있습니다.

# See Also

- [C# Programming Guide: Writing Text Files](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/file-system/how-to-write-to-a-text-file)
- [C# StreamWriter class documentation](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter)
- [C# File I/O operations tutorial](https://www.tutorialspoint.com/csharp/csharp_file_io.htm)