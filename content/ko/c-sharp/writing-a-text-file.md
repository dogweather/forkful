---
title:    "C#: 텍스트 파일 작성하기"
keywords: ["C#"]
---

{{< edit_this_page >}}

## 왜

어떤 이유로 사람들이 텍스트 파일을 작성하는지에 대해 이야기해보겠습니다. 텍스트 파일은 코드나 데이터를 저장하고 공유할 때 매우 유용합니다. 예를 들어, 여러분이 작성한 코드를 다른 개발자들과 공유하거나, 작업 내용을 저장하고 나중에 사용할 수 있도록 하는 등의 경우에 텍스트 파일을 작성하는 것이 필요합니다.

## 작성하는 방법

아래는 C# 언어를 사용하여 텍스트 파일을 작성하는 간단한 예제 코드와 출력 결과입니다. 코드 블록은 "```C# ... ```" 형식으로 표시되며, 각 코드의 설명은 한국어로 제공됩니다.

```C#
using System;  // 노출하려는 네임스페이스를 추가합니다.
using System.IO;  // 파일 처리를 위해 추가합니다.

// 텍스트 파일을 작성할 경로와 파일명을 지정합니다.
string filePath = @"C:\Users\Test\sample.txt";

// StreamWriter 클래스를 사용하여 파일을 생성하고 내용을 작성합니다.
using (StreamWriter writer = new StreamWriter(filePath))
{
    writer.WriteLine("Hello World!");  // "Hello World!"라는 내용을 파일에 작성합니다.
}

// 파일이 성공적으로 작성되었는지를 확인하기 위해 출력합니다.
Console.WriteLine("텍스트 파일이 성공적으로 작성되었습니다.");
```

출력 결과:
```
텍스트 파일이 성공적으로 작성되었습니다.
```

## 더 깊게 알아보기

텍스트 파일을 작성하는 것에 대해 더 자세히 알아보겠습니다. StreamWriter 클래스를 사용하면 파일의 인코딩 방식이나 한 줄씩 데이터를 추가하는 등의 여러 가지 옵션이 있습니다. 또한 데이터를 읽고 수정하는 등의 다양한 기능도 제공합니다. 이를 활용하여 더 복잡한 프로그램을 구현할 수 있습니다.

## 참고 자료

- [C# StreamReader and StreamWriter 클래스 바로 알기](https://blog.naver.com/vilgund/140105965864)
- [StreamWriter 클래스 개요](https://docs.microsoft.com/ko-kr/dotnet/api/system.io.streamwriter)
- [샘플 코드: 파일에 텍스트 쓰기 (C# 프로그래밍 가이드)](https://docs.microsoft.com/ko-kr/dotnet/csharp/programming-guide/file-system/how-to-write-to-a-text-file)