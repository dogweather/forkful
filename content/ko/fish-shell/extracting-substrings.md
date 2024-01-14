---
title:    "Fish Shell: 부분 문자열 추출하기"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## 왜

서브스트링을 추출하는 것은 Fish Shell 프로그래밍에서 매우 유용한 기능입니다. 예를 들어, 문자열에서 특정 부분을 선택하고 필요한 정보를 추출하는 등 다양한 상황에서 유용하게 사용할 수 있습니다.

## 방법

서브스트링을 추출하는 방법은 매우 간단합니다. 다음과 같은 코드를 사용하면 됩니다.

```Fish Shell
set my_string "안녕하세요, 한국어를 배우고 있습니다."
echo $my_string[10..14]
```

위 코드를 실행하면 "한국어"라는 서브스트링을 추출할 수 있습니다. 또한, 다음과 같이 위치를 지정하여 추출할 수도 있습니다.

```Fish Shell
echo $my_string[3]
```

위 코드를 실행하면 문자열의 세 번째 문자인 "하"를 추출할 수 있습니다. 또한, 문자열의 일부분을 변수에 저장하여 다른 명령에 사용할 수도 있습니다. 예를 들어, 서브스트링을 추출해서 변수에 저장한 후, 이를 다른 명령어에서 사용할 수 있습니다.

```Fish Shell
set my_substring $my_string[10..14]
echo "저는 $my_substring를 배우고 있습니다."
```

위 코드를 실행하면 "저는 한국어를 배우고 있습니다."라는 결과를 볼 수 있습니다.

## 딥 다이브

Fish Shell에서 서브스트링을 추출하는 방법은 아주 간단하지만, 자세히 살펴보면 더 많은 사용법을 발견할 수 있습니다. 서브스트링을 추출할 때는 시작과 끝 위치를 지정할 수 있으며, 또한 인덱스를 사용하여 특정 문자나 단어를 추출할 수도 있습니다. 또한, Fish Shell에서는 정규표현식을 사용하여 더 정교한 서브스트링 추출이 가능합니다.

## 더 알아보기

* [Fish Shell 문서: 문자열](https://fishshell.com/docs/current/cmds/set.html#substrings)
* [Fish Shell 사용법](https://fishshell.com/docs/current/tutorial.html) 
* [정규표현식을 활용하여 문자열 추출하기](https://fishshell.com/docs/current/tutorial.html#working_with_strings_using_regular_expressions)

## 참고

Fish Shell에서 제공하는 서브스트링 추출 기능은 프로그래밍을 할 때 매우 유용합니다. 문자열에서 필요한 정보를 추출하는 작업이 필요하다면, 항상 서브스트링 추출 기능을 기억하고 활용할 수 있도록 해보세요.