---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:49.378451-07:00
description: "\uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\
  \uB85C \uBCC0\uD658\uD558\uBA74\uC11C \uB098\uBA38\uC9C0\uB294 \uC18C\uBB38\uC790\
  \uB85C \uC720\uC9C0\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uC774\uB7EC\
  \uD55C \uBB38\uC790\uC5F4 \uC870\uC791 \uC791\uC5C5\uC740 \uC0AC\uC6A9\uC790 \uC774\
  \uB984\uC774\uB098 \uC81C\uBAA9\uC744 \uAD00\uB840\uB098 \uBB38\uBC95\uC801 \uC815\
  \uD655\uC131\uC5D0 \uB530\uB77C \uD45C\uC2DC\uD558\uAE30 \uC704\uD574 \uC560\uD50C\
  \uB9AC\uCF00\uC774\uC158\uC5D0\uC11C \uD14D\uC2A4\uD2B8\uB97C \uD3EC\uB9F7\uD305\
  \uD558\uB294 \uB370 \uC720\uC6A9\uD569\uB2C8\uB2E4."
lastmod: '2024-03-13T22:44:55.022498-06:00'
model: gpt-4-0125-preview
summary: "\uBB38\uC790\uC5F4\uC758 \uCCAB \uAE00\uC790\uB97C \uB300\uBB38\uC790\uB85C\
  \ \uBCC0\uD658\uD558\uBA74\uC11C \uB098\uBA38\uC9C0\uB294 \uC18C\uBB38\uC790\uB85C\
  \ \uC720\uC9C0\uD558\uB294 \uAC83\uC744 \uB9D0\uD569\uB2C8\uB2E4. \uC774\uB7EC\uD55C\
  \ \uBB38\uC790\uC5F4 \uC870\uC791 \uC791\uC5C5\uC740 \uC0AC\uC6A9\uC790 \uC774\uB984\
  \uC774\uB098 \uC81C\uBAA9\uC744 \uAD00\uB840\uB098 \uBB38\uBC95\uC801 \uC815\uD655\
  \uC131\uC5D0 \uB530\uB77C \uD45C\uC2DC\uD558\uAE30 \uC704\uD574 \uC560\uD50C\uB9AC\
  \uCF00\uC774\uC158\uC5D0\uC11C \uD14D\uC2A4\uD2B8\uB97C \uD3EC\uB9F7\uD305\uD558\
  \uB294 \uB370 \uC720\uC6A9\uD569\uB2C8\uB2E4."
title: "\uBB38\uC790\uC5F4 \uB300\uBB38\uC790\uD654"
weight: 2
---

## 무엇 & 왜?
문자열의 첫 글자를 대문자로 변환하면서 나머지는 소문자로 유지하는 것을 말합니다. 이러한 문자열 조작 작업은 사용자 이름이나 제목을 관례나 문법적 정확성에 따라 표시하기 위해 애플리케이션에서 텍스트를 포맷팅하는 데 유용합니다.

## 방법:
자바의 표준 라이브러리는 문자열 전체를 한 번에 대문자로 만드는 직접적인 방법을 제공하지 않지만, 내장 메소드의 조합을 통해 이를 달성할 수 있습니다. 더 복잡한 요구 사항의 경우, Apache Commons Lang과 같은 제3자 라이브러리가 간단한 해결책을 제공합니다.

### 자바의 내장 메소드 사용하기
외부 라이브러리 없이 문자열을 대문자로 만들려면 문자열을 단어로 나누고, 각 단어의 첫 글자를 대문자로 만든 다음 다시 합칠 수 있습니다. 다음은 간단한 접근 방식입니다:

```java
public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = capitalizeWords(text);
        System.out.println(capitalizedText); // 출력: "Hello, World!"
    }

    public static String capitalizeWords(String str) {
        char[] chars = str.toLowerCase().toCharArray();
        boolean found = false;
        for (int i = 0; i < chars.length; i++) {
            if (!found && Character.isLetter(chars[i])) {
                chars[i] = Character.toUpperCase(chars[i]);
                found = true;
            } else if (Character.isWhitespace(chars[i]) || chars[i]=='.' || chars[i]=='\'') { 
                found = false;
            }
        }
        return String.valueOf(chars);
    }
}
```

이 코드 스니펫은 전체 문자열을 소문자로 변환한 다음 각 문자를 반복하면서 각 단어의 첫 글자를 대문자로 만듭니다. 공백, 마침표, 작은따옴표를 단어 구분자로 간주합니다.

### Apache Commons Lang 사용하기

Apache Commons Lang 라이브러리는 다양한 예외 사항과 구분자를 처리하는 `WordUtils.capitalizeFully()` 메소드로 더 우아한 해결책을 제공합니다:

```java
// 의존성 추가: org.apache.commons:commons-lang3:3.12.0

import org.apache.commons.text.WordUtils;

public class CapitalizeString {
    public static void main(String[] args) {
        String text = "hello, world!";
        String capitalizedText = WordUtils.capitalizeFully(text);
        System.out.println(capitalizedText); // 출력: "Hello, World!"
    }
}
```

이 메소드를 사용하려면 Apache Commons Lang 라이브러리를 프로젝트에 추가해야 합니다. 이 라이브러리 메소드는 각 단어의 첫 글자를 대문자로 만들 뿐만 아니라, 각 단어의 나머지 글자를 소문자로 변환하여 문자열 전체에 일관된 대문자 패턴을 보장합니다.
