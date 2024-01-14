---
title:                "C#: 문자열을 소문자로 변환하기"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## 왜
 왜 한국어로 된 블로그 포스트를 읽고 있는 당신이 문자열을 소문자로 변환하는 데 관심이 있을까요? 문자열을 소문자로 변환하는 것은 프로그래밍에서 자주 쓰이는 기능 중 하나입니다. 예를 들면, 사용자가 입력한 정보를 데이터베이스와 비교할 때 대소문자를 구분하지 않기 위해 문자열을 소문자로 변환하는 것이 필요할 수 있습니다. 자세한 내용은 아래에서 확인해보세요.

## 방법
```C#
var inputString = "Hello World!";
var lowerCaseString = inputString.ToLower();
Console.WriteLine(lowerCaseString);
```
위의 예제는 입력된 문자열을 소문자로 변환하는 가장 간단한 방법입니다. 입력된 문자열을 `ToLower()` 메소드를 사용하여 소문자로 변환한 후, 변환된 문자열을 출력하는 예제입니다. 결과로는 "hello world!"가 출력됩니다.

때로는 문자열 안에 있는 특정 부분만 소문자로 변환하고 싶을 수도 있습니다. 이때는 `Substring()` 메소드를 이용하면 됩니다.

```C#
var inputString = "Hello World!";
var titleCaseString = inputString.Substring(0,1).ToUpper() + inputString.Substring(1).ToLower();
Console.WriteLine(titleCaseString);
```

위의 예제는 입력된 문자열의 첫 글자는 대문자로, 나머지는 소문자로 변환하는 예제입니다. 출력 결과는 "Hello world!"가 됩니다.

## 깊숙한 곳을 파헤치기

문자열을 소문자로 변환하는 방법은 문자열을 처리하는 데 필수적인 기능이기 때문에 많이 사용됩니다. C#에서는 `ToLower()` 메소드를 이용하여 소문자로 변환할 수 있지만, 다른 프로그래밍 언어를 사용하는 경우에는 다른 방법을 사용해야할 수도 있습니다.

또한, 문자열을 소문자로 변환하는 과정에서 발생하는 성능 차이에 대해서도 깊게 알아보는 것이 중요합니다. 문자열이 많이 사용되는 프로그램에서는 성능 차이가 큰 요소가 될 수 있기 때문입니다. 이러한 성능 차이를 최소화하기 위해서는 어떤 방법이 있는지도 알아보세요.

## 관련자료

- 메소드 정보: https://docs.microsoft.com/ko-kr/dotnet/api/system.string.tolower?view=net-5.0
- 성능 비교: https://www.dotnetperls.com/tolower-optimization