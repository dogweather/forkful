---
title:    "Java: 문자열 대문자로 변환하기"
keywords: ["Java"]
---

{{< edit_this_page >}}

# 왜

자바 프로그래밍에서 문자열의 첫 글자를 대문자로 바꾸는 것이 필요한 경우가 있습니다. 예를 들어, 사용자의 이름을 나타내는 문자열이 주어졌을 때, 그 이름을 대문자로 시작해야 할 수도 있습니다.

# 어떻게

문자열의 첫 글자를 대문자로 바꾸는 방법은 간단합니다. 우선, String 클래스의 `substring()` 메서드를 사용해 첫 글자를 추출하고, `toUpperCase()` 메서드를 사용해 대문자로 바꿔줍니다. 마지막으로, 나머지 문자열을 `substring()` 메서드를 이용해 추출한 첫 글자를 제외한 나머지 문자열과 합쳐주면 됩니다. 아래는 이 과정을 보여주는 예시 코드입니다.

```Java
public class StringCapitalization {

    public static void main(String[] args) {
        // 입력받은 이름 문자열
        String name = "john smith";

        // 첫 글자 추출
        String firstLetter = name.substring(0, 1);

        // 대문자로 변환
        String capitalLetter = firstLetter.toUpperCase();

        // 나머지 문자열 추출
        String remainingLetters = name.substring(1);

        // 첫 글자를 대문자로 변환한 나머지 문자열과 합침
        String capitalizedName = capitalLetter + remainingLetters;

        System.out.println(capitalizedName); // John smith 출력
    }

}
```

# 딥 다이브

위의 예시 코드에서 사용된 `substring()`과 `toUpperCase()` 메서드는 String 클래스에 정의된 메서드입니다. `substring()` 메서드는 첫 번째 매개변수로 시작 인덱스, 두 번째 매개변수로 끝 인덱스를 받아 해당 범위의 문자열을 추출해 반환해줍니다. `toUpperCase()` 메서드는 단순히 문자열을 대문자로 변환시켜줍니다. 더 많은 정보는 Java API 문서를 참고하시기 바랍니다.

# 참고

- [Java API 문서](https://docs.oracle.com/javase/8/docs/api/)

--- 

# 관련 링크

- [자바 문자열 관련 메서드](https://www.geeksforgeeks.org/java-string-class/)
- [자바 API 사용법](https://arahansa.github.io/docs_korean/java/basics/api.html)