---
title:                "문자열 대문자화"
aliases: - /ko/java/capitalizing-a-string.md
date:                  2024-02-03T19:05:49.378451-07:00
model:                 gpt-4-0125-preview
simple_title:         "문자열 대문자화"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/java/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
