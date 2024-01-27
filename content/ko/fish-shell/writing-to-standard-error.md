---
title:                "표준 오류로 쓰기"
date:                  2024-01-19
html_title:           "Bash: 표준 오류로 쓰기"
simple_title:         "표준 오류로 쓰기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
isCJKLanguage:        true
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (무엇인가요? 왜쓰나요?)
표준 에러로 쓰기란 프로그램에서 에러 메시지를 표준 출력이 아닌 표준 에러 스트림에 보내는 것입니다. 프로그래머들이 이를 사용하는 이유는 에러 메시지와 일반 출력을 분리할 수 있어 디버깅과 로깅을 간편하게 하기 위해서입니다.

## How to: (어떻게 쓰나요?)
Fish Shell에서 표준 에러로 쓰는 예시입니다. 

```
Fish Shell

# 에러 메시지를 표준 에러(stderr)로 보냅니다.
echo "에러 발생: 파일을 찾을 수 없습니다." >&2

# 일반 메시지를 표준 출력(stdout)으로 보냅니다.
echo "작업을 계속합니다..."
```

출력 예시:

에러 메시지는 별도로 표시되며 표준 출력에는 영향을 주지 않습니다.

## Deep Dive (심층 분석)
표준 에러로의 쓰기는 UNIX 시스템 초기부터 있어왔습니다. 대안으로는 로그 파일에 직접 쓰거나 syslog 같은 로깅 시스템을 사용하는 방법이 있습니다. Fish Shell에서는 `>&2`를 사용하여 쉽게 표준 에러에 쓸 수 있으며, 이는 bash와 다른 쉘 스크립트 언어와의 호환성을 위해 설계된 것입니다.

## See Also (관련 자료)
- [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
- [Bash와의 비교](https://fishshell.com/docs/current/tutorial.html#tut_compatibility)
- [UNIX 표준 입출력 스트림](https://en.wikipedia.org/wiki/Standard_streams)
