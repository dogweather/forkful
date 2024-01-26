---
title:                "로깅"
date:                  2024-01-26T01:03:24.441799-07:00
model:                 gpt-4-1106-preview
simple_title:         "로깅"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/logging.md"
---

{{< edit_this_page >}}

## 무엇을, 왜 하는가?
로그 기록은 기본적으로 애플리케이션이 무엇을 하는지에 대한 기록을 남기는 것입니다—마치 일기장처럼요, 하지만 코드용으로 말이죠. 프로그래머들은 상태 변화, 시스템 이벤트, 까다로운 버그 등 세세한 부분을 추적하기 위해 로그를 사용합니다. 이를 통해 문제가 발생하는 것을 간과하지 않고 확인할 수 있습니다.

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