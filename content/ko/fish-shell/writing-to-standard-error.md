---
title:                "Fish Shell: 표준 오류에 쓰기"
simple_title:         "표준 오류에 쓰기"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## 왜?

프로그래밍을 할 때, 오류 메시지를 만나는 것은 흔한 일입니다. 그러나 대부분의 사람들은 이 오류 메시지를 단순히 무시하고 넘어가려고 합니다. 하지만 이 메시지를 적절하게 처리하고 더 나은 코드를 작성하는 데 유용하게 사용할 수 있습니다. 이때 오류 메시지를 표준 에러(standard error)에 쓰는 것이 중요합니다. 이렇게 함으로써 프로그래머는 오류 메시지를 더 쉽게 추적하고 디버깅할 수 있습니다.

## 방법

Fish Shell에서 표준 에러에 메시지를 쓰는 방법은 아주 간단합니다. 아래의 코드 블록을 따라하면 됩니다.

```Fish Shell
# 오류 메시지를 표준 에러에 쓰는 예제
echo "안녕하세요" >&2
```

위 예제에서 `>&2`는 오류 메시지를 표준 에러에 쓰는 부분입니다. 이렇게 함으로써 오류 메시지는 표준 출력과는 별개의 곳에 기록되기 때문에 디버깅하는 데 유용합니다.

## 깊이 알아보기

표준 에러에 메시지를 쓰는 것 외에도, Fish Shell에서는 오류 메시지에 색상을 추가하는 기능도 제공합니다. 이를 통해 어떤 종류의 오류인지 쉽게 구분할 수 있고, 더 쉽게 디버깅할 수 있습니다.

```Fish Shell
# 색상을 추가한 오류 메시지
echo (red "잘못된 값이 입력되었습니다.") >&2
```

위 예제에서 `red`는 오류 메시지에 적용할 색상을 지정하는 부분입니다. 여러 가지 색상 옵션을 사용할 수 있으며, 더 자세한 정보는 [공식 문서](https://fishshell.com/docs/current/index.html#color)를 참고하세요.

## 더 알아보기

표준 에러에 메시지를 쓰는 것은 프로그래밍에서 중요한 기술이며, 이를 사용하면 더 나은 디버깅을 할 수 있습니다. 하지만 오류 메시지를 적절하게 처리하지 않으면 오히려 디버깅이 복잡해질 수 있습니다. 따라서 실제 코드에서 오류 메시지에 표준 에러를 사용하고 그에 따른 디버깅 방법을 익혀두세요.

## 더보기

- [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
- [표준 에러의 중요성](https://medium.com/@D4V1de0m0n/caveats-of-error-handling-in-shell-scripts-aed66bd3af2f)
- [Shell 스크립트 디버깅 팁](https://zeltser.com/debugging-shell-scripts/)