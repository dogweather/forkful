---
title:                "Fish Shell: 표준 오류에 대한 쓰기"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜

프로그래밍을 하다보면 종종 프로그램이 예상치 못한 오류를 발견할 때가 있습니다. 이때, 오류 메시지가 표시되지 않고 무시되는 경우가 종종 있습니다. 이런 경우, 개발자가 오류를 더 빠르게 찾을 수 있도록 내부적으로 오류를 기록하고 추적할 수 있도록 해주는 것이 표준 오류 스트림이 사용되는 이유입니다.

## 방법

```fish
#!/usr/bin/env fish

# 오류를 기록할 파일을 만듭니다.
set error_log /tmp/error.log
touch $error_log

# 스크립트 실행 중 오류가 발생할 때, 표준 오류 스트림을 해당 파일에 추가합니다.
my_script 2>> $error_log

# 오류 기록이 완료되면 파일을 읽어 들여 출력해줍니다.
cat $error_log
```

위의 예제를 실행하면, `my_script`에서 발생한 모든 오류가 `/tmp/error.log` 파일에 추가되어 기록됩니다. 이후 스크립트를 실행한 후, 해당 파일을 읽어들이면 오류 내용을 확인할 수 있습니다.

## 깊이 파고들기

표준 오류 스트림은 표준 출력 스트림과 달리, 오류를 표시할 때에만 사용되는 특별한 스트림입니다. 따라서 표준 출력 스트림과 분리하여 오류를 기록하고 관리하는 것이 중요합니다. 또한, 표준 오류 스트림은 표준 출력 스트림처럼 화면에 출력되지 않고 오류 파일에만 기록되기 때문에, 언제나 파일을 확인해야만 오류를 알 수 있습니다.

## 또 다른 정보

**참고자료:**

- [Fish Shell 공식 문서](https://fishshell.com/docs/current/)

## 참고자료

- [Fish Shell 공식 문서](https://fishshell.com/docs/current/)