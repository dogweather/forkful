---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:32:55.417355-07:00
description: "C#\uC5D0\uC11C \uD45C\uC900 \uC624\uB958(stderr)\uB85C \uC4F0\uAE30\uB294\
  \ \uC5D0\uB7EC \uBA54\uC2DC\uC9C0\uC640 \uC9C4\uB2E8\uC744 \uC815\uADDC \uCD9C\uB825\
  (stdout)\uACFC \uBCC4\uB3C4\uB85C \uC9C0\uC2DC\uD558\uC5EC \uC0AC\uC6A9\uC790\uC640\
  \ \uAC1C\uBC1C\uC790\uAC00 \uC815\uC0C1 \uD504\uB85C\uADF8\uB7A8 \uCD9C\uB825\uACFC\
  \ \uC624\uB958 \uC54C\uB9BC\uC744 \uAD6C\uBCC4\uD560 \uC218 \uC788\uB3C4\uB85D \uD569\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB514\uBC84\uAE45\uACFC\
  \ \uB85C\uAE45\uC744 \uBCF4\uB2E4 \uD6A8\uC728\uC801\uC73C\uB85C \uD558\uC5EC \uC560\
  \uD50C\uB9AC\uCF00\uC774\uC158\uC758 \uC6D0\uD65C\uD55C \uC6B4\uC601\uACFC \uC720\
  \uC9C0\u2026"
lastmod: 2024-02-19 22:05:14.169199
model: gpt-4-0125-preview
summary: "C#\uC5D0\uC11C \uD45C\uC900 \uC624\uB958(stderr)\uB85C \uC4F0\uAE30\uB294\
  \ \uC5D0\uB7EC \uBA54\uC2DC\uC9C0\uC640 \uC9C4\uB2E8\uC744 \uC815\uADDC \uCD9C\uB825\
  (stdout)\uACFC \uBCC4\uB3C4\uB85C \uC9C0\uC2DC\uD558\uC5EC \uC0AC\uC6A9\uC790\uC640\
  \ \uAC1C\uBC1C\uC790\uAC00 \uC815\uC0C1 \uD504\uB85C\uADF8\uB7A8 \uCD9C\uB825\uACFC\
  \ \uC624\uB958 \uC54C\uB9BC\uC744 \uAD6C\uBCC4\uD560 \uC218 \uC788\uB3C4\uB85D \uD569\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uB514\uBC84\uAE45\uACFC\
  \ \uB85C\uAE45\uC744 \uBCF4\uB2E4 \uD6A8\uC728\uC801\uC73C\uB85C \uD558\uC5EC \uC560\
  \uD50C\uB9AC\uCF00\uC774\uC158\uC758 \uC6D0\uD65C\uD55C \uC6B4\uC601\uACFC \uC720\
  \uC9C0\u2026"
title: "\uD45C\uC900 \uC5D0\uB7EC\uC5D0 \uC4F0\uAE30"
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
