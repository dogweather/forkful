---
title:    "C#: 디렉토리가 존재하는지 확인하는 방법"
keywords: ["C#"]
---

{{< edit_this_page >}}

## 왜

디렉토리가 있는지 확인하는 것이 왜 중요한지 궁금하신가요? 디렉토리가 존재하는지 여부를 확인하는 것은 프로그래머에게 중요한 작업 중 하나입니다. 파일 또는 폴더를 생성하기 전에 먼저 해당 디렉토리가 존재하는지 여부를 확인하는 것이 좋은 습관입니다. 이를 통해 런타임 중 에러를 방지하고 안정적인 프로그램을 제작할 수 있습니다.

## 디렉토리 존재 여부 확인하는 방법

```C#
using System;
using System.IO;

public class Program
{
    public static void Main()
    {
        string directoryPath = @"C:\Users\Username\Desktop\MyFolder";

        // 디렉토리가 존재하는지 확인하는 메소드
        bool directoryExists = Directory.Exists(directoryPath);

        // 결과 출력
        if (directoryExists)
        {
            Console.WriteLine($"디렉토리 {directoryPath}\n은 존재합니다.");
        }
        else
        {
            Console.WriteLine($"디렉토리 {directoryPath}\n은 존재하지 않습니다.");
        }
    }
}

// 예상 출력 결과
// 디렉토리 C:\Users\Username\Desktop\MyFolder
// 은 존재하지 않습니다.
```

## 깊이 알아보기

C#에서 디렉토리 존재 여부를 확인하는 방법은 간단합니다. 여기서 사용한 `Directory.Exists()` 메소드는 문자열 형태의 디렉토리 경로를 매개변수로 받아 해당 디렉토리가 존재하는지 여부를 `bool` 형태로 반환합니다. 이 메소드를 통해 디렉토리 존재 여부를 빠르게 확인할 수 있으며, 에러를 방지하기 위한 좋은 방법입니다.

## 참고

- [Directory.Exists() 메소드 문서](https://docs.microsoft.com/ko-kr/dotnet/api/system.io.directory.exists?view=net-5.0)
- [C#에서 디렉토리 존재 여부 확인하기 (블로그 포스트)](http://blog.naver.com/yourname/1234567890)