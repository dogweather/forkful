---
title:                "Fish Shell: 디버그 출력 프린트"
simple_title:         "디버그 출력 프린트"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

디버그 출력을 프린트하는 이유는 매우 중요합니다. 디버그 출력은 어떤 코드가 어떻게 실행되는지를 알려주는 가장 쉽고 빠른 방법입니다. 코드를 디버그할 때 디버그 출력은 오류를 찾는 데 매우 유용합니다.

## 시작하기

Fish 쉘에서 디버그 출력을 프린트하는 방법은 매우 간단합니다. 아래 코드 블록을 보고 따라해보세요.

```Fish Shell
set debug_output true
echo "디버그 출력 시작"
```

위 코드를 실행하면 "디버그 출력 시작"이라는 문구가 터미널에 출력됩니다. 이렇게 간단하게 디버그 출력을 실행할 수 있습니다.

## 깊이 파고들기

디버그 출력은 오류를 찾는 데 매우 유용하지만, 너무 많은 디버그 출력을 활성화하면 코드가 너무 느려지게 될 수 있습니다. 따라서 디버그 출력을 사용하실 때에는 주의하세요. 또한, 다른 쉘에서도 디버그 출력을 사용하실 때에는 문법이 조금 다를 수 있으니 유의하셔야 합니다.

## 놀러가기

- https://fishshell.com/ - Fish 쉘 공식 홈페이지
- https://fishshell.com/docs/current/ - Fish 쉘 공식 문서
- https://github.com/fish-shell/fish-shell - Fish 쉘의 깃허브 저장소