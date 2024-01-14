---
title:                "C#: 표준 에러에 쓰는 방법"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

프로그래머들이 오류를 쓰는 이유는 때때로 프로그램에서 디버깅 또는 로깅에 유용하기 때문입니다.

## 어떻게

 ```C#
using System;

class Program
{
    static void Main(string[] args)
    {
        Console.Error.WriteLine("이것은 오류입니다!");

        // 콘솔에 다음과 같이 출력됩니다:
        // 이것은 오류입니다!
    }
}
```

## 깊이 파묻기

표준 에러는 진단 및 디버깅에 도움이 될 수 있습니다. 예를 들어, 프로그램이 중단되면 오류 메시지가 출력되어 원인을 파악하는 데 도움이 될 수 있습니다.

## 더 알아보기

- [C# 개발자 Docs - 표준 에러 스트림](https://docs.microsoft.com/ko-kr/dotnet/api/system.console.error)
- [Microsoft Docs - C# 프로그래밍 가이드](https://docs.microsoft.com/ko-kr/dotnet/csharp/programming-guide/)
- [C# 한국어 매뉴얼](https://csharpstudy.com/)