---
title:                "부분 문자열 추출하기"
html_title:           "Go: 부분 문자열 추출하기"
simple_title:         "부분 문자열 추출하기"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/go/extracting-substrings.md"
---

{{< edit_this_page >}}

# 무엇과 왜?
추출 서브스트링이란 무엇인지 알아보고, 왜 프로그래머들이 이를 하는지 알아보자.

추출 서브스트링이란 문자열 내에서 원하는 부분만을 추출하는 것을 의미한다. 예를 들어, "Hello World"라는 문자열에서 "World"만을 추출하는 것이다. 프로그래머들은 이를 사용하여 필요한 정보만을 추출하거나, 해당 문자열의 특정 부분만을 사용하여 작업을 수행하기 위해 추출 서브스트링을 사용한다.

# 어떻게:
```Go
str := "Hello World"
// "World"만을 추출하여 새로운 변수에 저장
subStr := str[6:]
// 출력: "World"
fmt.Println(subStr)
```

위의 예시 코드에서는 "Hello World"라는 문자열에서 인덱스가 6부터 끝까지의 문자열만을 추출하여 새로운 변수에 저장하는 것을 보여준다. 이를 실행하면 "World"라는 결과가 출력된다.

# 깊게 파헤치기:
추출 서브스트링을 사용하는 방법은 다양하지만, 이를 사용하는 프로그래머들은 대부분 문자열의 일부분만을 사용하여 작업을 수행해야 할 때 유용하게 이용한다. 이 기술은 문자열 처리에 유용하며, 특히 새로운 문자열을 만들고자 할 때 간편하게 사용할 수 있다.

추출 서브스트링은 다른 알고리즘과 비교하면 성능 면에서 약간 떨어지는 편이다. 따라서, 문자열 처리가 많은 프로그램에서는 다른 대안을 고려하는 것이 좋다.

문자열 추출 서브스트링의 구현 방법은 각 프로그래밍 언어마다 다를 수 있지만, 대부분의 언어에서는 문자열의 인덱스를 활용하여 해당 부분만을 추출하는 방법을 사용한다.

# 참고 자료:
- 문자열 추출 서브스트링에 대한 Go 언어 공식 문서: https://golang.org/pkg/strings/#example_Repeat
- 다른 문자열 처리 방법에 대한 비교: https://www.geeksforgeeks.org/string-slicing-python/