---
title:                "표준 오류로 쓰기"
html_title:           "Bash: 표준 오류로 쓰기"
simple_title:         "표준 오류로 쓰기"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 무엇이고 왜?
표준 에러에 쓰는 것은 프로그래머들이 디버깅 및 예외 처리에 유용한 정보를 쉽게 얻기 위해 사용하는 것입니다.

## 방법:
`Bash ...` 코드 블록 안에 코딩 예시와 샘플 출력이 있습니다.

```bash
#!/bin/bash
echo "표준 에러 출력" 1>&2
```

출력:
```
표준 에러 출력
```

## 깊이 파고들기:
표준 에러에 쓰는 것은 프로그래머들이 쉽게 디버깅할 수 있도록 만들어진 리눅스의 표준 규칙 중 하나입니다. 이전에는 프로그래머들이 오류를 확인하는 데에는 STDERR이나 표준 에러를 사용하지 않았습니다. 그러나 현재는 STDERR을 사용해 쉽게 디버깅할 수 있으며, 또한 STDERR과 STDOUT의 차이점을 이용하여 예외 처리를 보다 쉽게 할 수 있습니다. 프로그래밍 언어에 따라 STDERR이나 표준 에러를 사용하는 방식이 다를 수 있지만, 대부분의 경우 예외 처리나 디버깅에 매우 유용한 기능입니다.

## 관련 자료:
https://linux.die.net/man/2/write 에서 표준 에러와 관련된 더 많은 정보를 확인할 수 있습니다.