---
title:    "Java: 정규 표현식 사용하기"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/java/using-regular-expressions.md"
---

{{< edit_this_page >}}

#왜 정규식을 사용해야 하는가?
정규식은 문자열에서 원하는 패턴을 찾아내기 위해 사용되는 강력한 도구입니다. 예를 들어, 이메일 주소를 찾거나 특정 문자열을 바꾸는 등의 작업을 할 때 정규식은 매우 유용합니다. 또한 정규식은 문자열 처리에 있어서 더욱 효율적이며 유연하게 사용할 수 있습니다.

#어떻게 사용하나요?
정규식을 사용하기 위해서는 먼저 패턴을 작성해야 합니다. 이 패턴에는 원하는 문자 또는 패턴을 나타내는 특수 문자들이 포함될 수 있습니다. 그리고 해당 패턴을 적용할 문자열도 필요합니다. 예를 들어, 다음은 이메일 주소를 찾는 정규식의 예제 코드입니다.

```Java
String pattern = "[a-zA-Z0-9]+@[a-zA-Z0-9]+\\.[a-z]{2,3}";
String input = "example@email.com";

Pattern regex = Pattern.compile(pattern);
Matcher matcher = regex.matcher(input);

if(matcher.find()) {
    System.out.println("Matches found: " + matcher.group());
}
```

위 코드에서는 정규식 패턴으로 이메일 주소에 해당하는 문자열을 찾습니다. 이때, "group" 메소드를 사용하여 해당 문자열을 출력합니다. 실행 결과는 다음과 같이 나타납니다.

```
Matches found: example@email.com
```

또 다른 예제로는 주민등록번호 형식의 문자열에서 숫자 부분만 추출하는 정규식을 살펴보겠습니다.

```Java
String pattern = "\\d{6}-\\d{7}";
String input = "960101-1234567";

Pattern regex = Pattern.compile(pattern);
Matcher matcher = regex.matcher(input);

if(matcher.find()) {
    System.out.println("Number: " + matcher.group().replaceAll("-", ""));
}
```

위 코드에서는 정규식 패턴으로 "6자리 숫자-7자리 숫자" 형식을 찾아 숫자 부분만 출력하는 예제입니다. 실행 결과는 다음과 같습니다.

```
Number: 9601011234567
```

#더 깊이 들어가기
정규식은 강력한 도구이지만, 그만큼 복잡하고 이해하기 어려울 수 있습니다. 따라서, 정규식을 사용하기 전에 충분히 공부하고 연습하는 것이 중요합니다. 또한 정규식 패턴을 작성할 때에는 자주 사용되는 특수 문자들의 의미를 잘 알고 적절한 패턴을 사용하는 것이 중요합니다. 그리고 정규식을 잘 활용하기 위해서는 텍스트 처리에 관련된 다른 기술들을 함께 사용하는 것이 좋습니다. 예를 들어, 문자열을 가공하고 검색하며 정규식을 이용하여 비슷한 패턴을 한 번에 처리하는 등의 다양한 기술들을 함께 사용할 수 있습니다.

#관련 링크들
- [정규식 테스트 사이트](https://regexr.com)
- [정규식 패턴 빌더](https://www.regexbuilder.org)
- [정규식 연습 사이트](https://regexone.com)
- [정규식 문법 가이드](https://www.regular-expressions.info)
- [자주 사용되는 정규식 특수 문자들](https://www.regular-expressions.info/quickstart.html#chars)