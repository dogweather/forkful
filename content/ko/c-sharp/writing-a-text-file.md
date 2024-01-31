---
title:                "텍스트 파일 작성하기"
date:                  2024-01-19
html_title:           "Arduino: 텍스트 파일 작성하기"
simple_title:         "텍스트 파일 작성하기"

tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 파일 쓰기는 문자 데이터를 파일 시스템에 기록하는 과정입니다. 프로그래머가 로그, 설정, 보고서 저장 등을 위해 사용합니다. 

## How to: (어떻게 하나요?)
```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string filePath = "example.txt";
        string textToWrite = "안녕하세요, C# 파일 쓰기 예제입니다.";

        // 파일에 텍스트 쓰기
        File.WriteAllText(filePath, textToWrite);

        // 파일에 한 줄씩 텍스트 추가하기
        string[] lines = { "첫 번째 줄", "두 번째 줄" };
        File.WriteAllLines(filePath, lines);

        // StreamWriter 사용하여 파일에 쓰기
        using (StreamWriter writer = new StreamWriter(filePath, true))
        {
            writer.WriteLine("새로운 줄 추가");
        }

        // 결과 출력
        Console.WriteLine(File.ReadAllText(filePath));
    }
}
```
Sample Output:
```
첫 번째 줄
두 번째 줄
새로운 줄 추가
```

## Deep Dive (심층 분석)
- History: C# 초기버전부터 `System.IO` 네임스페이스가 파일 쓰기 기능을 제공합니다.
- Alternatives: `StreamWriter`, `File`, `FileInfo` 클래스 등을 활용할 수 있습니다.
- Implementation: 파일을 쓸 때는 파일 경로, 인코딩, 가용성 등을 고려해야 합니다. 예외 처리가 필수적입니다.

## See Also (더 보기)
- [File.WriteAllText Method Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.writealltext)
- [StreamWriter Class Documentation](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamwriter)
- [Understanding Stream in .NET](https://docs.microsoft.com/en-us/dotnet/standard/io/)
