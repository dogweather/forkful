---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:49.982956-07:00
description: "\uBC29\uBC95: \uC790\uBC14\uC5D0\uC11C \uC815\uADDC \uD45C\uD604\uC2DD\
  \uC744 \uC9C0\uC6D0\uD558\uB294 \uAC83\uC740 \uC8FC\uB85C `java.util.regex` \uD328\
  \uD0A4\uC9C0 \uC548\uC758 `Pattern`\uACFC `Matcher` \uD074\uB798\uC2A4\uB97C \uD1B5\
  \uD574\uC11C\uC785\uB2C8\uB2E4. \uB2E4\uC74C\uC740 \uBB38\uC790\uC5F4 \uB0B4\uC5D0\
  \uC11C \uB300\uC18C\uBB38\uC790\uB97C \uAD6C\uBD84\uD558\uC9C0 \uC54A\uACE0 \uB2E8\
  \uC5B4\uB97C \uCC3E\uC544 \uBAA8\uB4E0 \uBC1C\uC0DD \uC704\uCE58\uB97C \uCD9C\uB825\
  \uD558\uB294 \uAC04\uB2E8\uD55C \uC608\uC785\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.033185-06:00'
model: gpt-4-0125-preview
summary: "\uC790\uBC14\uC5D0\uC11C \uC815\uADDC \uD45C\uD604\uC2DD\uC744 \uC9C0\uC6D0\
  \uD558\uB294 \uAC83\uC740 \uC8FC\uB85C `java.util.regex` \uD328\uD0A4\uC9C0 \uC548\
  \uC758 `Pattern`\uACFC `Matcher` \uD074\uB798\uC2A4\uB97C \uD1B5\uD574\uC11C\uC785\
  \uB2C8\uB2E4."
title: "\uC815\uADDC \uD45C\uD604\uC2DD \uC0AC\uC6A9\uD558\uAE30"
weight: 11
---

## 방법:
자바에서 정규 표현식을 지원하는 것은 주로 `java.util.regex` 패키지 안의 `Pattern`과 `Matcher` 클래스를 통해서입니다. 다음은 문자열 내에서 대소문자를 구분하지 않고 단어를 찾아 모든 발생 위치를 출력하는 간단한 예입니다:

```java
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RegexExample {
    public static void main(String[] args) {
        String text = "Regex is great for parsing. Parsing with regex is powerful.";
        String wordToFind = "parsing";
        
        Pattern pattern = Pattern.compile(wordToFind, Pattern.CASE_INSENSITIVE);
        Matcher matcher = pattern.matcher(text);
        
        while (matcher.find()) {
            System.out.println("Found '" + matcher.group() + "' at position " + matcher.start());
        }
    }
}
```

출력:
```
'parsing'을(를) 위치 16에서 찾았습니다
'Parsing'을(를) 위치 31에서 찾았습니다
```

문자열을 분리하는 작업과 같은 경우, 정규 표현식을 사용한 `String` 클래스의 `split()` 메소드를 사용할 수 있습니다:

```java
public class SplitExample {
    public static void main(String[] args) {
        String text = "Java,Python,Ruby,JavaScript";
        String[] languages = text.split(",");
        
        for (String language : languages) {
            System.out.println(language);
        }
    }
}
```

출력:
```
Java
Python
Ruby
JavaScript
```

자바에서 정규 표현식을 사용할 때, 외부 라이브러리가 복잡한 작업을 단순화시킬 수 있는 경우가 있습니다. 자바에서 정규 표현식을 다루기 위한 인기 있는 서드파티 라이브러리 중 하나는 `Apache Commons Lang`입니다. `StringUtils`와 같은 유틸리티를 제공하여 일부 정규 표현식 작업을 더 간단하게 만듭니다. 다음은 이를 사용하여 부분 문자열의 일치 항목 수를 계산하는 방법입니다:

```java
import org.apache.commons.lang3.StringUtils;

public class CommonsLangExample {
    public static void main(String[] args) {
        String text = "Regex makes text processing easier. Processing text with regex is efficient.";
        String substring = "processing";
        
        int count = StringUtils.countMatches(text, substring);
        System.out.println("'" + substring + "'가 " + count + " 번 나타납니다.");
    }
}
```

Apache Commons Lang을 사용하기 위해서는 프로젝트에 추가해야 합니다. Maven을 사용하는 경우, `pom.xml`에 다음 의존성을 추가하세요:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-lang3</artifactId>
    <version>3.12.0</version> <!-- 최신 버전을 확인하세요 -->
</dependency>
```

출력:
```
'processing'이(가) 2 번 나타납니다.
```
