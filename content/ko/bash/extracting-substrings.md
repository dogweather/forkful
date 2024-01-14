---
title:    "Bash: 부분 문자열 추출하기"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## 왜?

이 글에서는 Bash를 사용하여 substrings을 추출하는 방법을 배우게 됩니다. 문자열의 일부분만 필요한 경우가 많기 때문에 substring 추출은 코드 작성에 있어서 매우 유용합니다.

## 추출하는 방법

Bash에서 substring을 추출하기 위해서는 `${변수:시작인덱스:길이}`와 같은 구문을 사용합니다. 다음의 예제를 통해 이를 살펴보겠습니다.

```Bash
sentence="안녕하세요, 반가워요!"
echo ${sentence:0:3}
```

위 코드의 실행결과는 다음과 같습니다.

```
안녕하
```

`sentence` 변수의 인덱스 0부터 3개의 글자를 추출한 것을 볼 수 있습니다. 이를 통해 원하는 범위의 substring을 쉽게 추출할 수 있습니다.

## 깊이있게 알아보기

Bash에서 substring을 추출할 때 유의해야 할 몇가지 사항이 있습니다. 먼저, 시작 인덱스는 0부터 시작하며, 마지막 인덱스는 실제 길이보다 1 작은 값을 사용해야 합니다. 또한, 시작 인덱스와 길이를 생략할 경우 변수의 전체 값이 출력됩니다.

추출한 substring을 다시 변수에 저장할 수도 있습니다. 예를 들어, 위의 예제에서 추출한 substring을 `greeting` 변수에 저장하고 싶다면 다음과 같이 작성할 수 있습니다.

```Bash
sentence="안녕하세요, 반가워요!"
greeting=${sentence:0:3}
echo $greeting
```

추출한 substring을 다른 명령어의 인자로 사용할 수도 있습니다. 예를 들어, `grep` 명령어를 사용하여 substring이 포함된 라인만 출력할 수 있습니다.

```Bash
text="Hello, my name is John Doe."
substring="John"
echo $text | grep $substring
```

위의 코드의 실행결과는 다음과 같습니다.

```
Hello, my name is John Doe.
```

substring이 포함된 라인이 출력된 것을 볼 수 있습니다.

## 또 다른 정보들

Bash에서 서브스트링을 추출하는 방법 외에도 다양한 문자열 조작 기능을 제공합니다. `sed` 명령어를 사용하면 문자열을 치환하거나 삭제하는 등 더욱 다양한 작업을 할 수 있습니다. 또한, 문자열 조작에 대한 더 많은 정보가 필요하다면 Bash 공식 문서를 참고하는 것도 좋은 방법입니다.

## 관련 정보

[Bash 문자열 조작 방법](https://www.tldp.org/LDP/abs/html/textops.html)

[Bash 공식 문서](https://www.gnu.org/software/bash/)