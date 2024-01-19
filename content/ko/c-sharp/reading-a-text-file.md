---
title:                "텍스트 파일 읽기"
html_title:           "Bash: 텍스트 파일 읽기"
simple_title:         "텍스트 파일 읽기"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 무엇인가요 & 왜 하나요?

텍스트 파일 읽기는 파일의 내용을 문자열로 변환하는 프로세스입니다. 프로그래머가 이를 통해 데이터를 로드하고 분석할 수 있어 사용합니다.

## 방법은?

아래는 C#에서 텍스트 파일을 읽는 간단한 코드입니다.

```C#
using System;
using System.IO;

class Program {
    static void Main() {
        string text = File.ReadAllText(@"C:\Example\test.txt");
        Console.WriteLine(text);
    }
}
```
이 코드는 'C:\Example\test.txt'의 모든 텍스트를 읽어서 콘솔에 출력합니다.

## 딥 다이브

텍스트 파일 읽기는 오래전부터 프로그래밍에 기본적인 기능 중 하나였습니다. 처음 컴퓨터 시스템이 나왔을 때부터 이는 데이터를 저장하고 전송하는 기본적인 방법이었습니다.

대안으로 MemoryStream, StreamReader 등 다양한 클래스를 사용할 수 있습니다. 이들 각각은 특정 상황에서 유용하며, 적절한 방법을 선택하는 것이 중요합니다.

`File.ReadAllText`는 내부적으로 `StreamReader`를 이용해서 파일을 읽습니다. 이 메소드는 텍스트 파일의 내용을 문자열로 불러오고, 잘못된 경로나 권한 문제 등이 있을 때는 예외를 발생시킵니다.

## 참고 자료

- [Microsoft: File.ReadAllText Method](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.readalltext)
- [Microsoft: Reading Text From Files](https://docs.microsoft.com/en-us/dotnet/standard/io/how-to-read-text-from-a-file)
- [Microsoft: StreamReader Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader)