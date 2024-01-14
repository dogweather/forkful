---
title:    "C#: 텍스트 파일 읽기"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## 왜?

텍스트 파일을 읽는 것에 대해 왜 관심을 가질까? 텍스트 파일은 프로그램에서 많이 사용되는 기본적인 데이터 저장 방식입니다. 따라서 프로그래밍을 공부하는 데 있어서 텍스트 파일을 읽는 기술은 매우 중요합니다.

## 어떻게?

아래 예제 코드를 통해 C#에서 텍스트 파일을 읽는 방법을 알아보겠습니다.

```C#
using System;
using System.IO;

class Program
{
    static void Main()
    {
        // 텍스트 파일 경로 지정
        string filePath = "sample.txt";

        // 파일을 읽기 위한 StreamReader 객체 생성
        using (StreamReader sr = new StreamReader(filePath))
        {
            string line;

            // 파일의 모든 내용을 한 줄씩 읽어서 출력
            while ((line = sr.ReadLine()) != null)
            {
                Console.WriteLine(line);
            }
        }
    }
}
```

위 코드를 실행하면 `sample.txt` 파일의 내용이 한 줄씩 출력됩니다. 만약 파일의 내용이 숫자일 경우, `int.Parse()` 함수를 사용하여 문자열을 정수형으로 변환할 수 있습니다.

```C#
// 파일의 내용을 정수형으로 변환하여 출력
Console.WriteLine(int.Parse(line));
```

## 깊게 들어가기

텍스트 파일을 읽는 방법에 대해 더 깊이 알아보겠습니다. `StreamReader` 클래스는 `Read()` 메서드를 제공하여 파일의 내용을 `char` 배열로 읽을 수 있습니다. 이를 활용하면 파일을 특정 바이트 수만큼 읽어서 처리할 수 있는 장점이 있습니다.

또한, `File` 클래스의 `ReadAllLines()` 메서드를 사용하면 파일의 모든 내용을 한 번에 `string` 배열로 읽을 수 있습니다.

## 참고 자료

- [Microsoft Docs - StreamReader Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.streamreader?view=netcore-3.1)
- [Microsoft Docs - File Class](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=netcore-3.1)
- [C#에서 텍스트 파일 다루기](https://twpower.github.io/65-read-and-write-file-in-csharp)

## 관련 링크

- [C#에서 텍스트 파일 쓰기](https://github.com/jaydenkoh/csharp-text-file-write-ko)
- [C#으로 CSV 파일 다루기](https://github.com/jaydenkoh/csharp-csv-file-ko)
- [C# 파일 입출력 강좌](https://csharpstudy01.tistory.com/10)