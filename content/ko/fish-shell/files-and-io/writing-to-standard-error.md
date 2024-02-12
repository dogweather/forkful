---
title:                "표준 에러에 쓰기"
aliases:
- /ko/fish-shell/writing-to-standard-error/
date:                  2024-02-03T19:33:19.791839-07:00
model:                 gpt-4-0125-preview
simple_title:         "표준 에러에 쓰기"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## 무엇이며 왜?

Fish Shell에서 표준 오류(stderr)로 쓰기는 오류 메시지나 진단을 표준 출력(stdout)과 별개로 지시하는 것에 관한 것입니다. 프로그래머들은 오류 정보를 쉽게 식별, 관리하거나 리디렉션하여 디버깅 및 로깅 프로세스를 원활하게 하기 위해 이를 수행합니다.

## 방법:

Fish Shell에서는 `>&2`를 사용하여 출력을 리디렉션함으로써 stderr에 쓸 수 있습니다. 기본 예제는 다음과 같습니다:

```fish
echo "This is an error message" >&2
```

이 명령은 단순히 메시지를 stdout 대신 stderr에 에코합니다. 정규 및 오류 메시지를 모두 출력하는 스크립트를 작성한다면, 다음과 같이 할 수 있습니다:

```fish
echo "Starting the process"
echo "An error occurred" >&2
echo "Process completed"
```

스크립트를 실행하고 stderr를 파일로 리디렉션하면 다음과 같은 샘플 출력이 나타납니다:

```
Starting the process
Process completed
```

오류 메시지는 표준 출력에 나타나지 않지만 stderr을 리디렉션한 파일에서 찾을 수 있습니다.

더 정교한 오류 처리나 로깅이 필요한 시나리오에서 Fish는 이를 위해 명시적으로 설계된 내장 라이브러리를 제공하지 않습니다. 그러나, 외부 도구를 활용하거나 함수를 작성하여 도울 수 있습니다. 예를 들어, 간단한 로깅 함수를 만드는 것은 다음과 같아 보일 수 있습니다:

```fish
function log_error
    echo $argv >&2
end

log_error "This is an advanced error message"
```

이 함수 `log_error`는 주어진 모든 문자열을 받아 stderr에 쓸 것입니다. 이와 같은 함수를 사용하면 스크립트 전반에 걸쳐 오류 처리를 깔끔하고 일관되게 유지하는 데 도움이 됩니다.
