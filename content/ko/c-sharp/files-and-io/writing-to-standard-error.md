---
title:                "표준 에러에 쓰기"
aliases:
- ko/c-sharp/writing-to-standard-error.md
date:                  2024-02-03T19:32:55.417355-07:00
model:                 gpt-4-0125-preview
simple_title:         "표준 에러에 쓰기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇 & 왜?
C#에서 표준 오류(stderr)로 쓰기는 에러 메시지와 진단을 정규 출력(stdout)과 별도로 지시하여 사용자와 개발자가 정상 프로그램 출력과 오류 알림을 구별할 수 있도록 합니다. 프로그래머들은 디버깅과 로깅을 보다 효율적으로 하여 애플리케이션의 원활한 운영과 유지 관리를 가능하게 하기 위해 이렇게 합니다.

## 방법:
C#에서 표준 오류로 쓰기는 `Console.Error` 스트림을 사용하여 달성할 수 있습니다. 이 스트림은 에러 메시지와 진단을 위해 특별히 사용됩니다. 여기 간단한 예가 있습니다:

```csharp
Console.Error.WriteLine("Error: 요청 처리에 실패했습니다.");
```

표준 오류로의 샘플 출력:
```
Error: 요청 처리에 실패했습니다.
```

`Serilog`이나 `NLog`와 같이 고급 로깅 기능을 제공하는 타사 라이브러리를 사용하는 시나리오에서는 이러한 라이브러리를 구성하여 stderr로 오류 로그를 쓸 수 있습니다. 이 예제들은 단순한 콘솔 리디렉션에 초점을 맞추고 있지만, 생산 애플리케이션에서 로깅 프레임워크는 훨씬 더 견고한 오류 처리 및 출력 옵션을 제공한다는 것을 기억하세요. `Serilog`를 사용한 간단한 예제는 다음과 같습니다:

먼저, Serilog 패키지와 그것의 Console sink를 설치합니다:

```
Install-Package Serilog
Install-Package Serilog.Sinks.Console
```

그 다음, Serilog를 stderr로 쓰도록 구성합니다:

```csharp
using Serilog;

Log.Logger = new LoggerConfiguration()
    .WriteTo.Console(standardErrorFromLevel: Serilog.Events.LogEventLevel.Error)
    .CreateLogger();

Log.Information("이것은 일반 메시지입니다.");
Log.Error("이것은 에러 메시지입니다.");
```

에러 메시지에 대한 표준 오류로의 샘플 출력:
```
[15:04:20 ERR] 이것은 에러 메시지입니다.
```

참고: Serilog의 콘솔 싱크에서 `standardErrorFromLevel` 구성은 지정된 레벨(이 경우 Error) 또는 그보다 높은 모든 로그 이벤트를 표준 오류 스트림으로 리디렉션하며, 정보와 같은 낮은 레벨의 메시지는 표준 출력 스트림으로 쓰여집니다.
