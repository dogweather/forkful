---
title:                "문자열 보간"
html_title:           "PowerShell: 문자열 보간"
simple_title:         "문자열 보간"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ko/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

# 무엇 & 왜?

문자열 치환(interpolating)이란 무엇인가요? 간단하게 말하자면 변수의 값을 문자열 안에 삽입하는 것입니다. 프로그래머들이 이를 하는 이유는 코드를 더 간결하고 효율적으로 만들기 위해서입니다.

# 방법:

PowerShell에서 문자열 치환을 하는 방법은 `"$변수"`의 형식을 사용하는 것입니다. 아래의 예제를 참고해보세요.

```PowerShell
$name = "Jane"
"안녕하세요, $name님!"  # 출력: 안녕하세요, Jane님!
```

변수 대신에 식(expression)을 사용할 수도 있습니다.

```PowerShell
$value = 10
"식의 결과는 $($value * 2)입니다."  # 출력: 식의 결과는 20입니다.
```

# 깊이 들어가기:

문자열 치환은 1970년대에 Perl에서 처음 사용되었습니다. 다른 프로그래밍 언어들에서도 비슷한 기능을 제공합니다. C언어에서는 `printf()` 함수를, Ruby에서는 `#{}`을 이용하여 문자열을 치환합니다.

만약 PowerShell에서 변수가 아닌 텍스트로 인식되길 원한다면, 변수 앞에 백틱(`)을 붙여주면 됩니다. 아래의 예제를 참고해보세요.

```PowerShell
$name = "Jane"
"안녕하세요, `$name님!"  # 출력: 안녕하세요, $name님!
```

# 참고 자료:

- [PowerShell: How do I convert a String to an Int or Vice Versa?](https://stackoverflow.com/questions/1176165/how-do-i-convert-a-string-to-an-int-or-vice-versa)
- [PowerShell String Interpolation vs String.Format - Which one should you use?](https://www.adamtheautomator.com/powershell-string-interpolation/)
- [PowerShell Official Documentation: About Quoting Rules](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_quoting_rules?view=powershell-7.1)