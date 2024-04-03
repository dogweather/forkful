---
date: 2024-01-26 01:00:45.594458-07:00
description: "\uC0AC\uC6A9 \uBC29\uBC95: C#\uC5D0\uC11C\uB294 \uB0B4\uC7A5\uB41C `System.Diagnostics`\
  \ \uB124\uC784\uC2A4\uD398\uC774\uC2A4 \uB610\uB294 NLog\uB098 log4net \uAC19\uC740\
  \ \uD0C0\uC0AC \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD560 \uC218 \uC788\
  \uC2B5\uB2C8\uB2E4. \uB2E4\uC74C\uC740 .NET Core\uC5D0\uC11C \uC0AC\uC6A9 \uAC00\
  \uB2A5\uD55C `ILogger` \uC778\uD130\uD398\uC774\uC2A4\uB97C \uC0AC\uC6A9\uD558\uB294\
  \ \uAC04\uB2E8\uD55C \uC608\uC81C\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.243357-06:00'
model: gpt-4-1106-preview
summary: "C#\uC5D0\uC11C\uB294 \uB0B4\uC7A5\uB41C `System.Diagnostics` \uB124\uC784\
  \uC2A4\uD398\uC774\uC2A4 \uB610\uB294 NLog\uB098 log4net \uAC19\uC740 \uD0C0\uC0AC\
  \ \uB77C\uC774\uBE0C\uB7EC\uB9AC\uB97C \uC0AC\uC6A9\uD560 \uC218 \uC788\uC2B5\uB2C8\
  \uB2E4."
title: "\uB85C\uAE45"
weight: 17
---

## 사용 방법:
C#에서는 내장된 `System.Diagnostics` 네임스페이스 또는 NLog나 log4net 같은 타사 라이브러리를 사용할 수 있습니다. 다음은 .NET Core에서 사용 가능한 `ILogger` 인터페이스를 사용하는 간단한 예제입니다:

```C#
using Microsoft.Extensions.Logging;
using System;

public class Program
{
    public static void Main()
    {
        using var loggerFactory = LoggerFactory.Create(builder => {
            builder.AddConsole();
        });

        ILogger logger = loggerFactory.CreateLogger<Program>();

        logger.LogInformation("정보 메시지입니다.");
        logger.LogWarning("경고 메시지입니다.");
        logger.LogError("오류 메시지입니다.");
    }
}
```

샘플 출력:
```
info: Program[0]
      정보 메시지입니다.
warn: Program[0]
      경고 메시지입니다.
fail: Program[0]
      오류 메시지입니다.
```

## 심층 분석
소프트웨어 개발에서 로깅의 역사는 프로그래밍 자체만큼 오래되었으며, 단순한 출력 문에서부터 복잡하고 설정 가능한 시스템으로 발전해 왔습니다. 원래 로깅은 파일이나 콘솔에 작성하는 것으로 이루어졌지만, 로그 집계 시스템이나 분산 추적 플랫폼(예: ELK 스택 또는 자이거)과 같은 보다 복잡한 구조를 포함하도록 발전했습니다.

.NET의 내장 로깅에 대한 대안으로 타사 라이브러리가 있습니다:
- **NLog**: 설정하기 쉽고 라우팅, 형식 지정, 로그 필터링을 위한 많은 기능을 가진 다기능 라이브러리입니다.
- **log4net**: Java의 log4j 라이브러리에서 영감을 받아 XML에서 매우 설정 가능하며 다양한 로그 저장소를 지원합니다.

구현 세부 사항과 관련하여, 로깅 추상화(예: Microsoft.Extensions.Logging) 및 밑바탕 로깅 제공자의 선택은 응용 프로그램의 성능과 신뢰성에 중대한 영향을 미칠 수 있습니다. 로깅 레벨을 적절하게 구성하고 로그 작성이 병목 현상이 되지 않도록 하는 것이 중요합니다.

또한, 구조화된 로깅 - 단순한 문자열뿐만 아니라 키-값 쌍이나 객체를 로깅하는 것 - 은 조회하고 분석하기 더 쉬우며 좀 더 정확하고 실질적인 로그를 가능하게 합니다.

## 관련 정보
- [Microsoft.Extensions.Logging 문서](https://docs.microsoft.com/ko-kr/aspnet/core/fundamentals/logging/)
- [NLog 문서](https://nlog-project.org/documentation/)
- [log4net 문서](https://logging.apache.org/log4net/)
- [Serilog 문서](https://serilog.net/) (구조화된 로깅의 예)
