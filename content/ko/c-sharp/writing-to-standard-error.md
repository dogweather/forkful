---
title:                "표준 오류에 쓰는 방법"
html_title:           "C#: 표준 오류에 쓰는 방법"
simple_title:         "표준 오류에 쓰는 방법"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 무엇 & 왜?
표준 에러 메시지를 출력하는 것은 프로그래머가 디버깅을 할 때 유용합니다. 이를 통해 어떤 오류가 발생했는지를 더 자세히 알 수 있고, 문제를 해결하기 위한 방향을 잡을 수 있습니다.

## 방법:
```C#
Console.Error.WriteLine("표준 에러 메시지를 출력합니다.");
```

출력 예시:
```
표준 에러 메시지를 출력합니다.
```

## 딥 다이브:
(1) 표준 에러 메시지를 출력하는 기능은 오래 전부터 존재합니다. 초기에는 터미널에서 오류 메시지를 확인하기 위해 사용되었고, 지금은 프로그래밍 언어에서도 자주 사용됩니다. (2) 표준 에러 메시지 대신에 로그 파일을 생성하는 것도 일반적인 대안입니다. 하지만 더 많은 작업이 필요하며, 프로그램의 성능에도 영향을 줄 수 있습니다. (3) C#에서는 Console.Error.WriteLine 메서드를 사용하여 표준 에러 메시지를 출력할 수 있습니다. 이 메서드는 외부 리소스에 대한 핸들을 선점하지 않기 때문에 프로그램의 성능에 영향을 미치지 않습니다.

## 참고:
- [C# Console.Error.WriteLine 메서드 문서](https://docs.microsoft.com/en-us/dotnet/api/system.console.error.writeline)
- [표준 에러 스트림에 대한 자세한 설명](https://www.tutorialspoint.com/process_standard_error_stream/stderr_stream.htm)