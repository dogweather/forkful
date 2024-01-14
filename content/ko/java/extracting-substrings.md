---
title:    "Java: 부분 문자열 추출하기"
keywords: ["Java"]
---

{{< edit_this_page >}}

# 왜 substring 추출을 사용해야 할까?

substring 추출은 문자열을 다루는 프로그래밍에서 매우 유용한 기능입니다. 어떤 한 문자열에서 원하는 부분만을 추출하여 다른 문자열을 만들 수 있기 때문에 많은 상황에서 활용할 수 있습니다. 예를 들어, 한국어로 된 문자열에서 한 문장만을 추출하여 번역 프로그램에 사용하거나, 웹 스크래핑을 할 때 필요한 정보만을 추출하여 데이터를 가공하는 등 다양한 상황에서 유용하게 사용할 수 있습니다.

# 방법: substring 추출하는 방법

Java에서 substring 추출하는 방법은 간단합니다. "```Java
str.substring(beginIndex, endIndex)
```"와 같이 사용하면 됩니다. 여기서 beginIndex는 추출하고자 하는 문자열의 시작 인덱스, endIndex는 끝 인덱스를 의미합니다. 또한 endIndex는 생략 가능하며 생략하는 경우 자동으로 문자열의 끝까지 추출하게 됩니다.

예를 들어, "Hello World"라는 문자열에서 "Hello"만을 추출하려면 "```Java
str.substring(0, 5)
```"와 같이 사용하면 됩니다. 이렇게 추출된 substring은 새로운 문자열 객체로 반환됩니다. 따라서 원본 문자열에는 변화가 없다는 것을 유의해야 합니다.

# 딥 다이브: substring 추출의 깊은 이해

substring 추출은 indexOf나 lastIndexOf와 함께 사용하여 문자열에서 특정 부분을 찾고 그 부분을 추출할 수 있는 강력한 기능입니다. 또한 substring 추출을 여러 번 연결하여 새로운 문자열을 만들 수도 있습니다. 예를 들어 "Hello World"라는 문자열에서 "o Wo"만을 추출하고 싶은 경우 "```Java
str.substring(str.indexOf("o"), str.indexOf("d") + 1)
```"와 같이 사용하면 됩니다. 이렇게 하면 "o"와 "d"의 인덱스를 찾아내어 그 사이에 있는 부분을 추출하게 됩니다.

또한 substring 추출은 정규 표현식과 결합하여 특정 패턴을 가진 문자열만을 추출할 수도 있습니다. 예를 들어, "Hello1World2"라는 문자열에서 숫자만을 추출하고 싶은 경우 "```Java
str.substring(str.indexOf("\\d+"))
```"와 같이 정규 표현식을 제공하여 추출할 수 있습니다.

# 참고 자료

- [Java String class documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java substring method documentation](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#substring-int-int-)
- [Java regular expression tutorial](https://regexone.com/lesson/introduction_abcs)