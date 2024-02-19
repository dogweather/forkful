---
aliases:
- /ko/c-sharp/reading-a-text-file/
date: 2024-01-20 17:54:07.600537-07:00
description: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30\uB294 \uD30C\uC77C\uC5D0\
  \uC11C \uBB38\uC790 \uB370\uC774\uD130\uB97C \uAC00\uC838\uC624\uB294 \uACFC\uC815\
  \uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC0AC\uC6A9\uC790\
  \ \uB370\uC774\uD130 \uCC98\uB9AC, \uC124\uC815 \uC77D\uAE30, \uB610\uB294 \uB85C\
  \uADF8 \uBD84\uC11D \uB4F1\uC744 \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\
  \uD569\uB2C8\uB2E4."
isCJKLanguage: true
lastmod: 2024-02-18 23:09:06.247704
model: gpt-4-1106-preview
summary: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30\uB294 \uD30C\uC77C\uC5D0\uC11C\
  \ \uBB38\uC790 \uB370\uC774\uD130\uB97C \uAC00\uC838\uC624\uB294 \uACFC\uC815\uC785\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC0AC\uC6A9\uC790 \uB370\
  \uC774\uD130 \uCC98\uB9AC, \uC124\uC815 \uC77D\uAE30, \uB610\uB294 \uB85C\uADF8\
  \ \uBD84\uC11D \uB4F1\uC744 \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD569\
  \uB2C8\uB2E4."
title: "\uD14D\uC2A4\uD2B8 \uD30C\uC77C \uC77D\uAE30"
---

{{< edit_this_page >}}

## What & Why? (무엇과 왜?)
텍스트 파일 읽기는 파일에서 문자 데이터를 가져오는 과정입니다. 프로그래머들은 사용자 데이터 처리, 설정 읽기, 또는 로그 분석 등을 위해 이 작업을 수행합니다.

## How to: (방법)
```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        string path = "example.txt";

        // 파일 전체를 읽는 방법
        string readText = File.ReadAllText(path);
        Console.WriteLine(readText);

        // 파일을 라인별로 읽는 방법
        string[] lines = File.ReadAllLines(path);
        foreach (var line in lines)
        {
            Console.WriteLine(line);
        }

        // 파일을 스트림으로 읽는 방법
        using (StreamReader reader = new StreamReader(path))
        {
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                Console.WriteLine(line);
            }
        }
    }
}
```
```plaintext
// example.txt 내용을 읽으면 나오는 출력:
안녕하세요.
이것은 테스트 파일입니다.
파일 읽기 예제에 사용되었습니다.
```

## Deep Dive (심화 탐구)
초기 컴퓨팅 시대부터 파일 읽기는 중요했습니다. C#에는 여러 방식으로 텍스트 파일을 읽을 수 있으며, 위 예제는 그 중 간단한 세 가지를 보여줍니다. `File.ReadAllText` 메서드는 전체 파일의 내용을 메모리에 로드하여 편하나, 큰 파일 처리시 메모리 부담이 커질 수 있습니다. 이때는 `File.ReadAllLines`이나 `StreamReader`를 사용하여 라인별로 처리하는 것이 더 효율적입니다.

`StreamReader`는 내부적으로 버퍼를 사용하여 텍스트 읽기 성능을 최적화합니다. 또한, IDisposable 인터페이스를 구현하므로 `using` 구문으로 자원 관리를 자동화할 수 있습니다.

스트림을 직접 관리하기 원한다면 `FileStream`과 함께 `StreamReader`를 사용할 수도 있습니다. 대안으로는 `System.IO` 네임스페이스 안의 `MemoryMappedFile` 클래스를 통해 대용량 파일을 메모리에 매핑하고 부분적으로 읽는 방법도 있습니다.

## See Also (참고자료)
- [System.IO Namespace | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.io)
- [StreamReader Class | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader)
- [File and Stream I/O | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/standard/io/)
- [Memory-Mapped Files | Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/standard/io/memory-mapped-files)
