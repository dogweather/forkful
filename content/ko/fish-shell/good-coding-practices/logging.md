---
date: 2024-01-26 01:03:24.441799-07:00
description: "\uC5B4\uB5BB\uAC8C \uD574\uC57C \uD560\uAE4C\uC694? Fish\uC5D0\uC11C\
  \ \uB85C\uADF8 \uAE30\uB85D\uC740 \uD45C\uC900 \uCD9C\uB825 \uBC0F \uC624\uB958\
  \ \uC2A4\uD2B8\uB9BC\uC744 \uD30C\uC77C\uB85C \uB9AC\uB2E4\uC774\uB809\uC158\uD558\
  \uB294 \uAC83\uCC98\uB7FC \uAC04\uB2E8\uD560 \uC218 \uC788\uC2B5\uB2C8\uB2E4. \uC2A4\
  \uD06C\uB9BD\uD2B8\uC758 \uC2DC\uC791\uACFC \uC885\uB8CC \uC2DC\uAC04\uC744 \uB85C\
  \uADF8\uC5D0 \uAE30\uB85D\uD574\uBD05\uC2DC\uB2E4."
lastmod: '2024-03-13T22:44:55.865583-06:00'
model: gpt-4-1106-preview
summary: "Fish\uC5D0\uC11C \uB85C\uADF8 \uAE30\uB85D\uC740 \uD45C\uC900 \uCD9C\uB825\
  \ \uBC0F \uC624\uB958 \uC2A4\uD2B8\uB9BC\uC744 \uD30C\uC77C\uB85C \uB9AC\uB2E4\uC774\
  \uB809\uC158\uD558\uB294 \uAC83\uCC98\uB7FC \uAC04\uB2E8\uD560 \uC218 \uC788\uC2B5\
  \uB2C8\uB2E4."
title: "\uB85C\uAE45"
weight: 17
---

## 어떻게 해야 할까요?
Fish에서 로그 기록은 표준 출력 및 오류 스트림을 파일로 리다이렉션하는 것처럼 간단할 수 있습니다. 스크립트의 시작과 종료 시간을 로그에 기록해봅시다.

```fish
function log_start
  echo (date "+%Y-%m-%d %H:%M:%S") " - 스크립트 시작" >> my_app.log
end

function log_end
  echo (date "+%Y-%m-%d %H:%M:%S") " - 스크립트 종료" >> my_app.log
end

log_start
# ... 스크립트의 작업들 ...
log_end

cat my_app.log
```

`my_app.log`에 기록된 내용입니다:

```
2023-04-01 10:35:47  - 스크립트 시작
2023-04-01 10:36:02  - 스크립트 종료
```

고급 로깅을 위해서는 로그 레벨과 메시지를 매개변수로 하는 함수를 사용할 수 있습니다:

```fish
function log_message --argument message
  switch "$argv[1]"
    case 'INFO' 'WARN' 'ERROR'
      set log_level $argv[1]
    case '*'
      set log_level 'DEBUG'
  end
  set log_msg (string join " " $argv[2..-1])
  echo (date "+%Y-%m-%d %H:%M:%S") "[$log_level]" $log_msg >> my_app.log
end

log_message INFO "정보성 메시지입니다."
log_message ERROR "문제가 발생했습니다!"
```

`my_app.log`의 샘플 출력은 다음과 같습니다:
```
2023-04-01 10:35:47 [INFO] 정보성 메시지입니다.
2023-04-01 10:35:49 [ERROR] 문제가 발생했습니다!
```

## 심층 탐구
역사적으로 쉘 스크립트에서 로그 기록은 많은 `echo` 문들을 사용하여 수행되었고, 이 방법은 여전히 옵션으로 남아 있지만, 복잡한 시스템을 구현하는 것은 도전적일 수 있습니다. Fish는 다른 쉘들이나 프로그래밍 언어들처럼 내장된 로깅 메커니즘을 가지고 있지 않기 때문에, 종종 사용자가 직접 만들기도 합니다.

Fish의 내장된 `echo` 명령어 대신 로깅에 사용할 수 있는 대안으로는, 유닉스 도구들인 `syslog` 또는 `logger` 등이 있으며, 이를 통해 시스템 로그 데몬과 인터페이스를 제공하여 시스템 전체의 이벤트 로깅을 좀 더 통합된 방식으로 접근할 수 있습니다.

Fish의 단순함은 로깅의 장황함을 다루는 함수를 만드는 것을 허용하며, 켜고 끌 수 있는 다양한 레벨을 설정할 수 있게 합니다. 일부 구현에서는 스크립트 이름, 줄 번호, 타임스탬프를 포함할 수 있어 사건으로 이어진 단계를 더 쉽게 추적할 수 있습니다.

## 참고
- 함수 작성에 관한 Fish Shell 문서: https://fishshell.com/docs/current/#syntax-function
- 쉘 스크립트 작성 팁: https://developer.ibm.com/tutorials/l-lpic1-103-4/
- Syslog 프로토콜 가이드: https://tools.ietf.org/html/rfc5424
