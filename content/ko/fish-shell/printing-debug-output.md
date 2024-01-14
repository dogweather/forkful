---
title:    "Fish Shell: 디버그 출력 작성"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## 왜

디버그 출력을 인쇄하는 것에 대해 생각해보았을 때, 그것이 왜 중요한지 궁금하신가요? 디버그 출력은 코드를 디버그하는데 매우 유용합니다. 이를 통해 어떤 변수가 어떤 값을 가지고 있는지, 어떤 코드가 실행되고 있는지 등을 확인할 수 있습니다. 따라서 디버그 출력은 코딩 과정에서 중요한 역할을 합니다.

## 사용 방법

Fish Shell에서 디버그 출력을 하기 위해서는 어떻게 해야 할까요? 간단한 코딩 예제를 통해 알아보겠습니다.

```Fish Shell
set var "Hello World"
echo $var
```

위 예제에서는 "Hello World"라는 값을 가지는 변수를 선언하고, 그 값을 출력하고 있습니다. 이렇게 출력된 값으로부터 어떤 로직이 실행되고 있는지 파악할 수 있습니다. 다른 간단한 예제를 살펴보겠습니다.

```Fish Shell
for i in (seq 1 10)
  if [ $i -lt 5 ]
    echo $i
  else
    echo "Over 5"
  end
end
```

위 예제는 1부터 10까지의 숫자를 반복적으로 출력하는 예제입니다. 그리고 조건문을 통해 출력되는 값이 5보다 작을 경우에는 숫자를, 그렇지 않은 경우에는 "Over 5"라는 문자열을 출력하고 있습니다. 이렇게 코드를 실행하면서 디버그 출력을 통해 각 단계에서 어떤 동작이 이루어지고 있는지 확인할 수 있습니다.

## 깊게 파보기

디버그 출력에 대해 더 깊이 알아보겠습니다. Fish Shell에서는 다양한 방법으로 디버그 출력을 할 수 있습니다. 위에서 살펴본 echo 명령어 외에도 다음과 같은 방식으로 디버그 출력을 할 수 있습니다.

- printf 명령어: C언어와 유사한 형식의 문자열을 출력할 때 사용됩니다.
- set (or set -l) 명령어: 변수의 값을 출력할 때 사용됩니다.
- test 명령어: 조건문에서 조건을 체크하고 그에 맞는 결과를 출력할 때 사용됩니다.

이 외에도 여러 가지 명령어를 사용하여 디버그 출력을 할 수 있습니다. 따라서 코딩하시다가 어떤 상황에서 디버그 출력을 하는 것이 좋을지 고려해보시기 바랍니다.

## 더 알아보기

"Dealing with Debug Output in Fish Shell" by Caleb Ancell: https://calebancell.com/blog/debugging-fish-shell/

"Using Debug Output with Conditional Expressions" by Fish Shell documentation: https://fishshell.com/docs/current/cmds/test.html#using-debug-output-with-conditional-expressions

"Debugging with Fish Shell" by Derek Wyatt: https://medium.com/@derekwyatt/debugging-with-fish-shell-13c42504b99a

## 참고 자료

- ["디버거의 기본 개념" by 네이버 지식백과](https://terms.naver.com/entry.nhn?docId=1130211&cid=40942&categoryId=32220)
- ["디버깅 시작하기" by W3C Korea](https://schoo-nexon.w3c.or.kr/REFERENCE/w3c/TR/CSS21/)
- ["코딩하는 선생님: 벌써 알고 있는 내용은 빼주세요!" by 용준승](https://brunch.co.kr/@juneyoung/207)