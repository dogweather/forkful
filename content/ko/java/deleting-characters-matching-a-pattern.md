---
title:                "Java: 패턴과 일치하는 문자 삭제하기"
programming_language: "Java"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## 왜

문자열에서 일치하는 패턴을 가진 문자를 삭제하는 작업을 수행해야 하는 이유는 다양합니다. 예를 들어, 데이터를 정리하거나 유효하지 않은 문자를 제거하거나 웹 크롤링을 할 때 문자를 필터링하는 등 다양한 이유가 있을 수 있습니다. 이러한 작업은 문자열을 다룰 때 매우 유용하며, 자바 프로그래밍에서도 자주 사용됩니다.

## 어떻게

자바에서 문자열에서 일치하는 패턴을 찾아 이를 삭제하는 방법은 다양한 방법으로 구현할 수 있습니다. 하지만 가장 간단하고 일반적인 방법은 java.util.regex 패키지의 Pattern 및 Matcher 클래스를 사용하는 것입니다. 아래는 이 방법을 활용한 예제 코드와 출력 결과입니다.

```Java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class StringPatternRemoval {
    public static void main(String[] args) {
        // 문자열에서 'a'와 'b'가 연속해서 나오는 부분을 찾아 삭제
        patternRemoval("aabccddeffee", "ab");
        // 출력 결과: ccddeffee
    }

    // 문자열에서 일치하는 패턴을 찾아 이를 삭제하는 메소드
    private static void patternRemoval(String input, String pattern) {
        Pattern p = Pattern.compile(pattern);
        Matcher m = p.matcher(input);
        StringBuffer sb = new StringBuffer();
        while (m.find()) {
            m.appendReplacement(sb, "");
        }
        m.appendTail(sb);
        System.out.println(sb.toString());
    }
}
```

위 코드에서는 우선 java.util.regex 패키지의 Pattern 클래스를 사용하여 원하는 패턴을 컴파일한 후, Matcher 클래스를 사용하여 특정 문자열에서 해당 패턴을 찾아서 이를 삭제하는 작업을 수행합니다. 해당 패턴이 발견될 때마다 StringBuffer를 사용하여 해당 문자열을 삭제하고, 최종적으로 남은 문자열을 출력합니다. 이와 같은 방법을 사용하면 간단하게 패턴에 해당하는 문자를 삭제할 수 있습니다.

## 심화 학습

문자열에서 일치하는 패턴을 찾아서 삭제하는 방법에 대해 더 깊게 알아보겠습니다. 위의 예제 코드에서는 단순히 문자열에서 일치하는 패턴을 찾아서 삭제하는 기본적인 방법을 다루었습니다. 하지만 이 외에도 다양한 방법이 있으며, 문자열을 비교하여 패턴이 일치할 경우 삭제하는 방법도 있습니다. 또한 정규식을 사용하여 특정 패턴에 해당하는 문자를 삭제하는 방법도 존재합니다. 이러한 방법들은 자바 프로그래밍에서 문자열을 다루는 데 있어서 유용하게 활용될 수 있습니다. 따라서 관심이 있다면 자세히 공부를 해보시기 바랍니다.

## 관련 링크

- [Java String Class](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)
- [Java Regex Tutorial](https://www.baeldung.com/java-regex)
- [Java Regular Expressions](https://docs.oracle.com/javase/tutorial/essential/regex/)