---
title:    "Bash: 정규식 사용하기"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## 왜

정규 표현식을 사용하는 이유는 복잡한 문자열 검색 및 대체를 쉽게 할 수 있기 때문입니다.

## 어떻게

정규 표현식을 사용하기 전에, 우선 Bash 내장 명령인 grep과 sed를 사용하는 방법을 알아보겠습니다. 이 두 가지 명령은 정규 표현식을 사용하여 문자열을 검색하고 대체하는 데 유용한 명령입니다.

```Bash
# 문자열에서 'apple'을 검색하는 예제
$ grep 'apple' fruits.txt
# 'apple'을 'banana'로 대체하는 예제
$ sed -i 's/apple/banana/g' fruits.txt
```

이제 정규 표현식을 사용하는 방법을 알아보겠습니다. 정규 표현식은 문자열 내에서 특정 패턴을 검색하고 대체할 수 있도록 해주는 패턴 매칭 기술입니다. 좀 더 자세한 내용은 아래의 "딥 다이브" 섹션에서 다루겠습니다.

## 딥 다이브

정규 표현식은 다른 유용한 Bash 내장 명령인 awk와 함께 사용하는 것이 매우 유용합니다. awk는 파일 내에서 특정 패턴에 대한 작업을 수행하는 데 사용되며, 정규 표현식을 포함한 강력한 패턴 매칭 기능을 제공합니다.

패턴을 작성할 때 기억해야 할 몇 가지 중요한 포인트가 있습니다. 첫째, 정규 표현식에는 일반 문자, 메타 문자, 수량자 및 한정된 반복자가 있습니다. 둘째, 정규 표현식은 대소문자를 구분합니다. 세번째, 대괄호를 사용하여 패턴 안에서 문자 범위를 지정할 수 있습니다.

다음 예제에서는 파일 "fruits.txt"에서 'apple'이나 'banana'가 포함된 라인을 출력하는 방법을 보여줍니다.

```Bash
$ awk '/[ab]pple/' fruits.txt
```

더 많은 정규 표현식을 배우고 싶다면, 다른 언어와 마찬가지로 온라인 자료를 참고하는 것이 좋습니다.

## 참고 자료

- [Bash 정규 표현식 공식 문서](https://www.gnu.org/software/grep/manual/html_node/Regular-Expressions.html)
- [정규 표현식 테스트 사이트](https://regex101.com/)
- [Awk 튜토리얼](http://www.grymoire.com/Unix/Awk.html)