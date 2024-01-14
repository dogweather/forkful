---
title:    "Fish Shell: 문자열 연결"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## 왜
문자열을 이어붙이는 작업을 어째서할까요? 이 작업은 변수와 문자열을 결합하여 다양한 문자열을 만들 수 있는 매우 간단하면서도 유용한 방법입니다. 예를 들어, 사용자의 이름을 변수로 정의하고, 해당 변수를 이용하여 인사말을 만들어 낼 수 있습니다. 문자열 뿐만 아니라 변수, 함수 등 여러 가지를 결합하여 더 많은 기능을 내포한 문자열을 만들 수 있습니다.

## 하우 투
`Fish Shell(피쉬 쉘)`에서 문자열을 이어붙이는 작업은 매우 간단합니다. 먼저, 두 개의 문자열을 연결하기 위해서는 `string1+string2` 와 같은 형식으로 입력하면 됩니다. 예를 들어, 여러분의 이름을 포함한 인삿말을 출력하고 싶다면 다음과 같이 작성할 수 있습니다.

```Fish Shell
set name "홍길동"
echo "안녕하세요, " $name "님!"
```

위 코드에서 `set`은 변수를 정의하는 명령어이고, `echo`는 화면에 문자열을 출력하는 명령어입니다. 이를 실행하면 다음과 같은 결과가 나올 것입니다.

```
안녕하세요, 홍길동 님!
```

또한, `string1 $string2` 와 같은 형식으로도 이어붙일 수 있습니다. 이 경우에는 두 개의 문자열 사이에 공백이 들어가게 됩니다. 아래의 예시를 참고해보세요.

```Fish Shell
set str1 "I"
set str2 "love"
set str3 "Fish"
echo $str1 $str2 $str3
```

```
I love Fish
```

## 딥 다이브
문자열 이어붙이기 작업을 더 깊이 알아보면서, 다양한 방법과 활용법을 익힐 수 있습니다. 예를 들어, `string1 $string2` 외에도 `string1$string2`, `string1(| |)string2` 등 다양한 형식으로 문자열을 이어붙일 수 있습니다. 이를 통해 특정 문자열을 포함하거나, 공백을 포함하지 않고 이어붙이는 등 다양한 유형의 문자열을 만들 수 있습니다.

또한, 피쉬 쉘에서는 `string1..string2`와 같은 형식으로 두 개 이상의 변수나 함수를 이어붙일 수 있습니다. 이를 통해 변수나 함수 뿐만 아니라 다양한 형태의 데이터를 포함한 문자열을 만들 수 있습니다.

## See Also
- [Fish Shell 공식 문서](https://fishshell.com/docs/current/)
- [Fish Shell 사용법 블로그](https://medium.com/@baroquebeat/fish-shell-d5e4129be1e)
- <https://ko.wikipedia.org/wiki/%EC%97%AC%EA%B8%80_%EC%9D%80%ED%8F%AC%EA%B5%B0_%EC%BB%A4%EB%84%A5%ED%8C%85>