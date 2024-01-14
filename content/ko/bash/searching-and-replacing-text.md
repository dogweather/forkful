---
title:                "Bash: 텍스트 검색 및 교체"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# 왜?

이 글에서는 Bash 프로그래밍에서 텍스트를 검색하고 교체하는 방법을 알려 드리고자 합니다. 이는 문자열을 빠르게 수정하고 원하는 결과를 얻기 위해 필수적입니다.

## 어떻게 적용할까요?

우선, `sed` 명령어를 사용하여 텍스트를 검색하고 원하는 패턴으로 교체할 수 있습니다. 예를 들어, 다음과 같은 파일을 가정해보겠습니다.

```
Hello Bash Blog
This is an example of replacing text using sed.
```

이제 우리는 이 파일에서 "Bash"라는 단어를 "Python"으로 바꾸고 싶다고 가정해 봅시다. 우선, 다음의 코드를 실행합니다.

```Bash
sed -i 's/Bash/Python/' example.txt
```

위 코드는 파일의 모든 줄에서 "Bash"를 "Python"으로 바꿔주는 역할을 합니다. `-i` 옵션은 원본 파일을 수정하도록 해줍니다. 이제 예제 파일의 내용을 확인해보면 다음과 같이 변경되어 있을 것입니다.

```
Hello Python Blog
This is an example of replacing text using sed.
```

위의 예제 외에도, `sed` 명령어를 다양한 방식으로 조합하여 좀 더 복잡한 검색과 교체를 할 수 있습니다. 하지만 이는 이 글의 범위를 벗어나므로, 나중에 자세히 다루도록 하겠습니다.

## 깊이 파헤치기

`sed` 명령어는 매우 강력하지만, 보다 조금 더 복잡한 텍스트 검색 및 교체를 위해서는 정규표현식을 알아야 합니다. 정규표현식은 텍스트에서 패턴을 정의할 때 사용되는 특수한 문자열입니다. 예를 들어, `[0-9]`는 0부터 9까지의 숫자 중 하나를 의미하며, `*`는 그 앞의 문자가 0번 이상 반복되는 경우에 일치합니다.

반복문을 사용하여 `grep` 명령어를 이용해 정규표현식으로 텍스트를 검색하고 `sed` 명령어를 사용하여 교체하는 방식으로 조합하여 더 복잡한 작업도 처리할 수 있습니다. 이렇게 조합하는 것은 초기에는 어려울 수 있지만, 연습을 통해 익숙해지면 보다 빠른 텍스트 검색 및 교체가 가능해집니다.

## 관련 자료

- [Bash `sed` command tutorial (영문)](https://www.youtube.com/watch?v=RoKK6SEAsg4)
- [정규표현식을 이용한 텍스트 검색 및 교체 (영문)](https://www.regular-expressions.info)
- [Bash 명령어 여러 가지 (영문)](https://www.digitalocean.com/community/tutorials/using-grep-regular-expressions-to-search-for-text-patterns-in-linux)