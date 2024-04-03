---
date: 2024-01-26 03:40:12.541978-07:00
description: "\uC5B4\uB5BB\uAC8C: \uC6B0\uB9AC\uC758 \uD14D\uC2A4\uD2B8\uC5D0\uC11C\
  \ \uADF8 \uC131\uAC00\uC2E0 \uB530\uC634\uD45C\uB97C \uC81C\uAC70\uD574\uBD05\uC2DC\
  \uB2E4. \uAC04\uB2E8\uD55C \uC218\uC815\uC744 \uC704\uD574 `replace()` \uBA54\uC18C\
  \uB4DC\uB97C \uC0AC\uC6A9\uD558\uACE0, \uBCF5\uC7A1\uD55C \uACBD\uC6B0\uB97C \uC704\
  \uD574 \uC815\uADDC\uC2DD(regex)\uB97C \uC0AC\uC6A9\uD560 \uAC83\uC785\uB2C8\uB2E4\
  ."
lastmod: '2024-03-13T22:44:55.030328-06:00'
model: gpt-4-0125-preview
summary: "\uC6B0\uB9AC\uC758 \uD14D\uC2A4\uD2B8\uC5D0\uC11C \uADF8 \uC131\uAC00\uC2E0\
  \ \uB530\uC634\uD45C\uB97C \uC81C\uAC70\uD574\uBD05\uC2DC\uB2E4."
title: "\uBB38\uC790\uC5F4\uC5D0\uC11C \uB530\uC634\uD45C \uC81C\uAC70\uD558\uAE30"
weight: 9
---

## 어떻게:
우리의 텍스트에서 그 성가신 따옴표를 제거해봅시다. 간단한 수정을 위해 `replace()` 메소드를 사용하고, 복잡한 경우를 위해 정규식(regex)를 사용할 것입니다.

```java
public class QuoteRemover {
    public static void main(String[] args) {
        String stringWithQuotes = "\"Hello, 'World'!\"";
        String withoutQuotes = stringWithQuotes.replace("\"", "").replace("'", "");
        System.out.println(withoutQuotes); // Hello, World!

        // 정규식을 사용한 패턴 애호가를 위해
        String stringWithMixedQuotes = "\"Java\" and 'Programming'";
        String cleanString = stringWithMixedQuotes.replaceAll("[\"']", "");
        System.out.println(cleanString); // Java and Programming
    }
}
```

## 심층 탐구
옛날에는 문자열에 따옴표가 큰 문제가 아니었습니다—시스템이 단순했고, 데이터도 그리 복잡하지 않았습니다. 그러나 JSON, XML과 같은 복잡한 데이터 포맷의 등장과 데이터 교환의 필요성으로 인해 따옴표 관리가 핵심이 되었습니다. 대안에 대해 말하자면, 각 문자를 순회하면서 새로운 문자열을 구축하는 파서를 작성할 수도 있겠죠(비 오는 날 재미있을 수 있습니다). 또한, 문자를 제거하는 대신 이스케이프하거나 로케일에 따라 다른 유형의 따옴표를 처리할 수 있는 옵션을 제공하는 보다 정교한 제3자 라이브러리도 있습니다. 구현 측면에서, 문맥 없이 따옴표를 제거하는 것은 데이터의 의미나 구조를 변경할 수 있으니 항상 "왜"를 고려한 후 "어떻게"를 고려하세요.

## 참조
- 정규식에 대한 보다 심층적인 탐구를 원한다면, 공식 Java 문서를 확인하세요: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/regex/Pattern.html
- 따옴표를 제거하는 대신 이스케이프해야 하나요? Stack Overflow가 도움을 줄 겁니다: https://stackoverflow.com/questions/383551/escape-string-for-sql-insert
- Java에서 JSON 처리를 해야 한다면? 따옴표를 자주 마주칠 것입니다. 시작점은 여기입니다: https://www.oracle.com/technical-resources/articles/java/json.html
