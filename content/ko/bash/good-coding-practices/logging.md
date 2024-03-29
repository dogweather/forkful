---
date: 2024-01-26 01:00:04.884623-07:00
description: "\uB85C\uAE45\uC740 \uD504\uB85C\uADF8\uB7A8\uC758 \uC2E4\uD589 \uD504\
  \uB85C\uC138\uC2A4\uC5D0\uC11C \uC774\uBCA4\uD2B8, \uC624\uB958 \uBC0F \uAE30\uD0C0\
  \ \uC911\uC694\uD55C \uC815\uBCF4\uB97C \uD30C\uC77C\uC774\uB098 \uCD9C\uB825 \uC2A4\
  \uD2B8\uB9BC\uC5D0 \uAE30\uB85D\uD558\uB294 \uAD00\uD589\uC785\uB2C8\uB2E4. \uD504\
  \uB85C\uADF8\uB798\uBA38\uB4E4\uC774 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC758\
  \ \uB3D9\uC791\uC744 \uCD94\uC801\uD558\uACE0, \uBB38\uC81C\uB97C \uB514\uBC84\uAE45\
  \uD558\uBA70, \uBBF8\uB798\uC758 \uBB38\uC81C \uD574\uACB0\uC5D0 \uB3C4\uC6C0\uC744\
  \ \uC904 \uC218 \uC788\uB294 \uC6B4\uC601\uC758 \uC5ED\uC0AC\uC801 \uAE30\uB85D\uC744\
  \ \uC720\uC9C0\uD558\uAE30 \uC704\uD574 \uB85C\uAE45\uC744 \uC2E4\uC2DC\uD569\uB2C8\
  \uB2E4."
lastmod: '2024-03-13T22:44:55.491229-06:00'
model: gpt-4-1106-preview
summary: "\uB85C\uAE45\uC740 \uD504\uB85C\uADF8\uB7A8\uC758 \uC2E4\uD589 \uD504\uB85C\
  \uC138\uC2A4\uC5D0\uC11C \uC774\uBCA4\uD2B8, \uC624\uB958 \uBC0F \uAE30\uD0C0 \uC911\
  \uC694\uD55C \uC815\uBCF4\uB97C \uD30C\uC77C\uC774\uB098 \uCD9C\uB825 \uC2A4\uD2B8\
  \uB9BC\uC5D0 \uAE30\uB85D\uD558\uB294 \uAD00\uD589\uC785\uB2C8\uB2E4. \uD504\uB85C\
  \uADF8\uB798\uBA38\uB4E4\uC774 \uC560\uD50C\uB9AC\uCF00\uC774\uC158\uC758 \uB3D9\
  \uC791\uC744 \uCD94\uC801\uD558\uACE0, \uBB38\uC81C\uB97C \uB514\uBC84\uAE45\uD558\
  \uBA70, \uBBF8\uB798\uC758 \uBB38\uC81C \uD574\uACB0\uC5D0 \uB3C4\uC6C0\uC744 \uC904\
  \ \uC218 \uC788\uB294 \uC6B4\uC601\uC758 \uC5ED\uC0AC\uC801 \uAE30\uB85D\uC744 \uC720\
  \uC9C0\uD558\uAE30 \uC704\uD574 \uB85C\uAE45\uC744 \uC2E4\uC2DC\uD569\uB2C8\uB2E4\
  ."
title: "\uB85C\uAE45"
---

{{< edit_this_page >}}

## 무엇 & 왜?

로깅은 프로그램의 실행 프로세스에서 이벤트, 오류 및 기타 중요한 정보를 파일이나 출력 스트림에 기록하는 관행입니다. 프로그래머들이 애플리케이션의 동작을 추적하고, 문제를 디버깅하며, 미래의 문제 해결에 도움을 줄 수 있는 운영의 역사적 기록을 유지하기 위해 로깅을 실시합니다.

## 방법:

Bash에서 로깅은 파일에 출력을 리다이렉트하거나 추가하는 것만큼 간단할 수 있습니다. 기본 예제는 다음과 같습니다:

```Bash
echo "스크립트 시작..." >> script.log
# 여기에 스크립트 명령어가 들어갑니다
echo "스크립트 $(date)에 완료됨" >> script.log
```

더 고급 기능이 필요하다면, 시스템 전체 로깅을 위해 syslog를 활용할 수 있습니다:

```Bash
logger "내 스크립트에서 온 사용자 정의 메시지"
```

`logger`는 syslog 서비스에 로그 메시지를 보내고, 그 후에 메시지는 시스템의 syslog 구성에 따라 처리됩니다.

`script.log`에서 포착된 샘플 출력:

```Bash
스크립트 시작...
2021년 3월 23일 화요일 09:26:35 PDT에 스크립트 완료됨
```

## 심층 탐구

역사적으로 Unix 계열 시스템에서는 로깅이 syslog 서비스에 의해 촉진되었으며, 다양한 애플리케이션과 시스템의 부분들이 메시지를 중앙에 로깅할 수 있도록 하였습니다. 이는 시스템 전체에 걸쳐 표준화된 로깅 메커니즘을 구현할 수 있게 합니다.

대안을 찾는 경우, 일부는 `syslog-ng`나 `rsyslog`를 사용하여 좀 더 고급 로깅 기능을 활용하거나, 분석 목적으로 시계열 데이터베이스에 로그를 기록할 수도 있습니다. Log4j(Java 생태계)나 Monolog(PHP)와 같이 구조화되고 구성 가능한 로깅 옵션을 제공할 수 있는 전용 로깅 라이브러리나 애플리케이션을 사용하는 것이 Bash와 같은 스크립팅 언어에 대해서도 복잡성이 높은 애플리케이션에 적합할 수 있습니다.

로깅을 구현하는 방법은 애플리케이션의 요구 사항에 크게 달려 있습니다. 단순히 스크립트 진행 상황을 추적하기 위한 출력이 필요한 경우, 파일에 메시지를 추가하는 것은 간편합니다. 그러나 좀 더 확장성 있고 견고한 로깅을 원한다면, 로그 회전, 로그 레벨 및 원격 로깅과 같은 기능을 지원하는 로깅 시스템과 통합할 것을 고려해야 합니다.

## 참고

- `logger` 및 `syslog` 함수에 대한 `man` 페이지는 항상 도움이 되므로, `man logger` 또는 `man syslog`를 시도해 보세요.
- 시스템 로깅에 대한 심층적인 이해를 원한다면 `rsyslog` 및 `syslog-ng` 문서를 참고하세요.
- Unix 계열 시스템에서의 로깅 배경과 원칙에 대해 더 알아보려면, RFC 5424에 문서화된 `Syslog` 프로토콜이 포괄적인 정보를 제공합니다.
