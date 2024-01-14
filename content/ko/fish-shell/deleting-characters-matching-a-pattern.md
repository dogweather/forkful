---
title:    "Fish Shell: 패턴과 일치하는 문자 삭제"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜
컴퓨터 작업을 하다 보면, 때로는 문자열에서 특정한 패턴과 일치하는 문자를 삭제해야 할 때가 있습니다. 이를 위해 Fish Shell 프로그래밍을 사용할 수 있습니다. 여기서는 왜 문자를 삭제해야 하는지 쉽게 이해할 수 있는 예시와 함께 소개할 것입니다.

## 하는 법
먼저, 우리는 다음과 같은 문자열을 가지고 있다고 가정하겠습니다.

```Fish Shell
set fruits apple banana orange
```

이제 fruits 변수의 값에서 "apple"이라는 단어를 삭제해보겠습니다. 이를 위해서는 다음과 같이 작성할 수 있습니다.

```Fish Shell
set fruits (echo $fruits | sed 's/apple//g')
```

위 코드를 실행하면 fruits 변수의 값에서 "apple"이 제거된 문자열이 결과로 출력될 것입니다.

```Fish Shell
set fruits banana orange
```

이와 유사하게, 여러 가지 패턴에 일치하는 문자를 한 번에 삭제할 수도 있습니다. 예를 들면, 다음과 같이 작성할 수 있습니다.

```Fish Shell
set fruits (echo $fruits | sed 's/[aeiou]//g')
```

위 코드는 fruits 변수의 값에서 모음을 모두 삭제할 것입니다. 그 결과는 다음과 같습니다.

```Fish Shell
set fruits bnn rng
```

이렇듯 문자를 삭제하는 방법은 다양하게 존재합니다. 이를 참고해서 스스로 도전해보세요!

## 딥 다이브
이제 문자를 삭제하는 방법을 간단하게 알아봤습니다. 그렇지만 이제 좀 더 깊이 들어가서 문자를 삭제하는 방법을 더 자세히 알아보겠습니다.

우선, 위에서 사용한 sed 명령어는 스트림 편집기로서 문자열에서 특정 패턴과 일치하는 부분을 삭제하는 데 사용됩니다. 이 명령어의 구성은 다음과 같습니다.

```Fish Shell
sed 's/패턴/대체할 문자열/플래그'
```

여기서 패턴은 일치하는 부분을 지칭하고, 대체할 문자열은 해당 부분을 대체할 문자열을 의미합니다. 마지막으로 플래그는 대체할 문자열을 모두 다른 대상으로 바꾸거나, 전체 도큐먼트에서 패턴 일치하는 모든 부분을 삭제하도록 지정하는 역할을 합니다.

따라서 sed 명령어를 활용하면 다양한 패턴에 일치하는 문자를 일괄적으로 삭제할 수 있습니다.

## 예시
- https://www.gnu.org/software/sed/manual/sed.html
- https://fishshell.com/docs/current/index.html#language

## 참고
- [sed command tutorial](https://www.youtube.com/watch?v=_vugqXEukNU)
- [Fish Shell documentation](https://fishshell.com/docs/current/index.html)