---
title:    "C#: 부문 추출하기"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/ko/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

# 왜 

자바 프로그래밍에서 substring을 추출하는 것이 유용한 이유는 다양합니다. 예를 들어, 문자열에서 필요한 정보만 추출하고 싶을 때 사용할 수 있습니다. 또는 문자열 파싱에 유용하게 사용할 수 있습니다. substring은 문자열을 조작하는 데 매우 유용한 기능입니다.

## 어떻게 

자바에서 substring을 추출하는 것은 매우 간단합니다. 아래 코드는 입력 문자열에서 일부를 추출하는 방법을 보여줍니다.

```C#
string input = "안녕하세요, 한국의 독자 여러분!";
string result = input.Substring(5, 3); // 결과: "하세요"
Console.WriteLine(result);
```

입력 문자열 "안녕하세요, 한국의 독자 여러분!"에서 시작 위치 5부터 3개의 문자를 추출하여 결과 변수에 할당합니다. 그리고 이를 출력하는 코드입니다.

## 깊이 파헤치기 

substring은 문자열을 조작하는 데 매우 유용한 기능입니다. 그러나 실제로는 더 많은 기능을 가지고 있습니다. 예를 들어, 시작 위치를 지정하지 않고 문자열의 끝까지 추출할 수도 있습니다. 또한, 입력 문자열이 없는 경우 예외를 발생시키지 않고 빈 문자열을 반환합니다. 이러한 유용한 기능들을 잘 숙지하고 적절하게 활용하면 더욱 효율적인 코드를 작성할 수 있습니다.

# See Also 

- [String.Substring 메서드 문서 (MSDN)](https://docs.microsoft.com/ko-kr/dotnet/api/system.string.substring)
- [C# 문자열 다루기 (네이버 블로그)](https://blog.naver.com/PostView.nhn?blogId=cjs12242&logNo=221781237609&categoryNo=0&parentCategoryNo=0&viewDate=&currentPage=1&postListTopCurrentPage=1&from=search)
- [C# 자습서 - substring과 trim 사용 방법 (YouTube)](https://www.youtube.com/watch?v=mVvWbkvIhh8)