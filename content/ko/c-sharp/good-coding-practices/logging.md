---
date: 2024-01-26 01:00:45.594458-07:00
description: "\uB85C\uAE45\uC740 \uB7F0\uD0C0\uC784 \uB3D9\uC548 \uC560\uD50C\uB9AC\
  \uCF00\uC774\uC158\uC758 \uC774\uBCA4\uD2B8\uC640 \uB370\uC774\uD130 \uCD9C\uB825\
  \uC744 \uAE30\uB85D\uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\
  \uB798\uBA38\uB4E4\uC740 \uBC84\uADF8\uB97C \uC9C4\uB2E8\uD558\uACE0, \uC18C\uD504\
  \uD2B8\uC6E8\uC5B4 \uC131\uB2A5\uC744 \uBAA8\uB2C8\uD130\uB9C1\uD558\uACE0, \uC0AC\
  \uC6A9\uC790 \uD589\uB3D9\uC744 \uCD94\uC801\uD558\uBA70, \uBCF4\uC548 \uBC0F \uBE44\
  \uC988\uB2C8\uC2A4 \uD45C\uC900\uC744 \uC720\uC9C0\uD558\uAE30 \uC704\uD574 \uB85C\
  \uAE45\uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.243357-06:00'
model: gpt-4-1106-preview
summary: "\uB85C\uAE45\uC740 \uB7F0\uD0C0\uC784 \uB3D9\uC548 \uC560\uD50C\uB9AC\uCF00\
  \uC774\uC158\uC758 \uC774\uBCA4\uD2B8\uC640 \uB370\uC774\uD130 \uCD9C\uB825\uC744\
  \ \uAE30\uB85D\uD558\uB294 \uACFC\uC815\uC785\uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\
  \uBA38\uB4E4\uC740 \uBC84\uADF8\uB97C \uC9C4\uB2E8\uD558\uACE0, \uC18C\uD504\uD2B8\
  \uC6E8\uC5B4 \uC131\uB2A5\uC744 \uBAA8\uB2C8\uD130\uB9C1\uD558\uACE0, \uC0AC\uC6A9\
  \uC790 \uD589\uB3D9\uC744 \uCD94\uC801\uD558\uBA70, \uBCF4\uC548 \uBC0F \uBE44\uC988\
  \uB2C8\uC2A4 \uD45C\uC900\uC744 \uC720\uC9C0\uD558\uAE30 \uC704\uD574 \uB85C\uAE45\
  \uC744 \uC0AC\uC6A9\uD569\uB2C8\uB2E4."
title: "\uB85C\uAE45"
weight: 17
---

## 무엇 & 왜?
로깅은 런타임 동안 애플리케이션의 이벤트와 데이터 출력을 기록하는 과정입니다. 프로그래머들은 버그를 진단하고, 소프트웨어 성능을 모니터링하고, 사용자 행동을 추적하며, 보안 및 비즈니스 표준을 유지하기 위해 로깅을 사용합니다.

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
