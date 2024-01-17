---
title:                "텍스트 검색과 대체"
html_title:           "Python: 텍스트 검색과 대체"
simple_title:         "텍스트 검색과 대체"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## 무엇과 왜?

검색 및 대체 텍스트는 프로그래머가 특정 단어 또는 구문을 찾고 다른 단어 또는 구문으로 대체하는 것을 말합니다. 이는 많은 코드 작성 작업에서 필수적인 작업입니다. 소스 코드에서 많은 기능을 갖추고 있을 수 있기 때문입니다.

## 어떻게 하나요?

Python에서 검색 및 대체 텍스트를 수행하는 방법은 간단합니다. 다음은 코드 예시와 함께 나와 있습니다.

```Python
# "Hello World"를 "안녕하세요"로 대체하기
text = "Hello World"
new_text = text.replace("Hello", "안녕")
print(new_text) # 출력 결과: "안녕 World"
```

또 다른 예시를 보면 다음과 같습니다.

```Python
# 주소에서 "도로"를 "길"로 바꾸기
address = "서울시 강남구 도로 1212"
new_address = address.replace("도로", "길")
print(new_address) # 출력 결과: "서울시 강남구 길 1212"
```

## 깊게 들어가보기

검색 및 대체 텍스트는 매우 오래된 기술입니다. 예를 들어, 텍스트 에디터 Vim에서는 "substitute"라는 명령을 통해 이 기능을 제공합니다. 또한 다른 프로그래밍 언어에서도 검색 및 대체 기능을 지원합니다. 예를 들어, Java에서는 ```replace()``` 메소드를 제공합니다.

## 관련 자료

검색 및 대체 기능에 대해 더 알고 싶다면 다음 링크를 참조해보세요.

- [Python 공식 문서](https://docs.python.org/3/library/stdtypes.html#str.replace)
- [Vim 공식 문서](https://vim.fandom.com/wiki/Search_and_replace)
- [Java 공식 문서](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#replace-char-char-)