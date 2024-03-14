---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:31.168964-07:00
description: "\uC6F9 \uD398\uC774\uC9C0 \uB2E4\uC6B4\uB85C\uB4DC\uB294 URL\uC744 \uD1B5\
  \uD574 \uC6F9 \uD398\uC774\uC9C0\uC758 \uB0B4\uC6A9\uC744 \uAC00\uC838\uC640 \uCC98\
  \uB9AC\uD558\uAC70\uB098 \uC800\uC7A5\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC815\uBCF4\uB97C \uCD94\
  \uCD9C\uD558\uAC70\uB098 \uBCC0\uACBD \uC0AC\uD56D\uC744 \uBAA8\uB2C8\uD130\uB9C1\
  \uD558\uAC70\uB098 \uCF58\uD150\uCE20\uB97C \uC544\uCE74\uC774\uBE0C\uD558\uAE30\
  \ \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4. \uC774\uB294\
  \ \uC6F9 \uC2A4\uD06C\uB798\uD551, \uB370\uC774\uD130 \uB9C8\uC774\uB2DD \uBC0F\
  \ \uC790\uB3D9\uD654\uB41C \uD14C\uC2A4\uD305 \uC791\uC5C5\uC5D0\uC11C\u2026"
lastmod: '2024-03-13T22:44:54.785877-06:00'
model: gpt-4-0125-preview
summary: "\uC6F9 \uD398\uC774\uC9C0 \uB2E4\uC6B4\uB85C\uB4DC\uB294 URL\uC744 \uD1B5\
  \uD574 \uC6F9 \uD398\uC774\uC9C0\uC758 \uB0B4\uC6A9\uC744 \uAC00\uC838\uC640 \uCC98\
  \uB9AC\uD558\uAC70\uB098 \uC800\uC7A5\uD558\uB294 \uAC83\uC744 \uD3EC\uD568\uD569\
  \uB2C8\uB2E4. \uD504\uB85C\uADF8\uB798\uBA38\uB4E4\uC740 \uC815\uBCF4\uB97C \uCD94\
  \uCD9C\uD558\uAC70\uB098 \uBCC0\uACBD \uC0AC\uD56D\uC744 \uBAA8\uB2C8\uD130\uB9C1\
  \uD558\uAC70\uB098 \uCF58\uD150\uCE20\uB97C \uC544\uCE74\uC774\uBE0C\uD558\uAE30\
  \ \uC704\uD574 \uC774 \uC791\uC5C5\uC744 \uC218\uD589\uD569\uB2C8\uB2E4. \uC774\uB294\
  \ \uC6F9 \uC2A4\uD06C\uB798\uD551, \uB370\uC774\uD130 \uB9C8\uC774\uB2DD \uBC0F\
  \ \uC790\uB3D9\uD654\uB41C \uD14C\uC2A4\uD305 \uC791\uC5C5\uC5D0\uC11C\u2026"
title: "\uC6F9\uD398\uC774\uC9C0 \uB2E4\uC6B4\uB85C\uB4DC\uD558\uAE30"
---

{{< edit_this_page >}}

## 무엇 & 왜?

웹 페이지 다운로드는 URL을 통해 웹 페이지의 내용을 가져와 처리하거나 저장하는 것을 포함합니다. 프로그래머들은 정보를 추출하거나 변경 사항을 모니터링하거나 콘텐츠를 아카이브하기 위해 이 작업을 수행합니다. 이는 웹 스크래핑, 데이터 마이닝 및 자동화된 테스팅 작업에서 필수적인 부분입니다.

## 어떻게?

Dart는 HTTP 요청을 만들기 위한 인기 있는 서드파티 라이브러리인 `http` 패키지를 제공합니다. 다음은 그것을 사용하여 웹페이지를 다운로드하는 기본 예제입니다:

먼저, `pubspec.yaml`에 `http` 패키지를 추가합니다:

```yaml
dependencies:
  http: ^0.13.3
```

그런 다음, 패키지를 가져와서 웹 페이지의 내용을 가져옵니다:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var url = Uri.parse('http://example.com');
  var response = await http.get(url);
  if (response.statusCode == 200) {
    print('페이지 다운로드됨:');
    print(response.body);
  } else {
    print('요청 실패 상태: ${response.statusCode}.');
  }
}
```

**샘플 출력** (이는 웹 페이지의 내용에 따라 다를 것입니다):

```
페이지 다운로드됨:
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

쿠키 처리나 사용자 에이전트 헤더 설정과 같이 더 복잡한 시나리오의 경우, 동일한 `http` 패키지를 사용하지만 요청에 추가 구성을 사용합니다:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var headers = {
    'User-Agent': 'YourCustomUserAgent/1.0',
    'Cookie': 'name=value; name2=value2',
  };
  var url = Uri.parse('http://example.com');
  var response = await http.get(url, headers: headers);

  if (response.statusCode == 200) {
    print('커스텀 헤더로 다운로드된 페이지:');
    print(response.body);
  } else {
    print('요청 실패 상태: ${response.statusCode}.');
  }
}
```

이와 같은 헤더를 사용하면 브라우저 요청을 더 정확하게 모방할 수 있으며, 스크래핑에 대한 특정 요구 사항이나 보호 조치가 있는 사이트를 다룰 때 특히 유용합니다.
