---
title:                "랜덤 숫자 생성"
html_title:           "Fish Shell: 랜덤 숫자 생성"
simple_title:         "랜덤 숫자 생성"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## 왜
랜덤 숫자를 생성하는 일을 하는 이유는 무엇일까요? 우리는 일상생활에서 랜덤 숫자를 자주 사용합니다. 예를 들어, 랜덤으로 나오는 로또 번호를 기대하거나, 랜덤으로 할당되는 업무를 받는 등 다양한 상황에서 랜덤 숫자가 필요합니다. 따라서 랜덤 숫자를 프로그래밍으로 생성해주는 Fish Shell을 배우면 여러분의 일상생활에서 유용하게 활용할 수 있습니다.

## 방법
먼저, Fish Shell을 실행하고 다음과 같은 코드를 입력해보세요.
```Fish Shell
set -U random_number (math random)
echo $random_number
```
위의 코드를 실행하면 0과 1 사이의 랜덤한 숫자가 생성됩니다. 만약 특정 범위의 숫자를 생성하고 싶다면, 다음과 같이 코드를 수정할 수 있습니다.
```Fish Shell
set -U random_number (math random 1 10)
echo $random_number
```
위의 코드는 1부터 10 사이의 랜덤한 숫자를 생성합니다. 또한, 랜덤 숫자를 사용할 때마다 다른 값이 나오도록 하고 싶다면, 다음과 같이 코드를 수정할 수 있습니다.
```Fish Shell
begin;
set -U random_number (math random)
echo $random_number
end;
```
위의 코드는 매번 다른 랜덤 숫자를 생성하여 출력합니다. 따라서 여러분은 개발한 어플리케이션에 랜덤 숫자를 쉽게 적용할 수 있습니다.

## 깊이 파보기
Fish Shell에서는 ```math random``` 이외에도 여러 가지 랜덤 기능을 제공합니다. 예를 들어, ```math between```을 사용하면 특정 범위에서 랜덤한 정수를 생성할 수 있습니다. 또한, ```math shuffle```을 사용하면 리스트나 배열을 무작위로 섞을 수 있습니다. 이 외에도 더 다양한 랜덤 기능을 포함하고 있으니 여러분은 Fish Shell 공식 문서를 참고하여 더 많은 정보를 얻을 수 있습니다.

## 관련 링크들
- [Fish Shell 공식 문서](https://fishshell.com/docs/current/index.html)
- [Fish Shell GitHub 레포지토리](https://github.com/fish-shell/fish-shell)
- [Fish Shell 랜덤 기능 관련 문서](https://fishshell.com/docs/current/cmds/math.html#random-numbers)