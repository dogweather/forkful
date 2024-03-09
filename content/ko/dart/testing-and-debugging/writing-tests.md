---
title:                "테스트 코딩하기"
date:                  2024-03-08T21:58:22.908181-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## 무엇과 왜?

Dart에서 테스트를 작성한다는 것은 프로그램의 다른 부분들이 예상대로 작동하는지를 자동으로 확인하기 위해 테스트 케이스를 만드는 것을 의미합니다. 프로그래머들은 이러한 작업을 통해 자신들의 코드가 신뢰할 수 있고 결함이 없으며, 업데이트와 리팩토링을 쉽게 하고 회귀를 방지하기 위해 이를 수행합니다.

## 방법:

Dart에서는 `test` 패키지를 일반적으로 테스트 작성에 사용합니다. 먼저 `pubspec.yaml`에 `test` 패키지를 추가합니다:

```yaml
dev_dependencies:
  test: ^1.0.0
```

그 다음, 간단한 함수에 대한 테스트를 작성합니다. 두 숫자를 더하는 함수가 있다고 가정해 보겠습니다:

```dart
int add(int a, int b) {
  return a + b;
}
```

다음으로, `test` 디렉토리에 `add_test.dart`라는 파일을 생성하고 테스트 케이스를 작성합니다:

```dart
import 'package:test/test.dart';
import '../lib/add.dart'; // `add` 함수가 lib/add.dart에 있다고 가정합니다

void main() {
  test('두 숫자를 더함', () {
    var expected = 3;
    expect(add(1, 2), equals(expected));
  });
}
```

테스트를 실행하려면 Dart 명령어를 사용합니다:

```bash
$ dart test
```

샘플 출력은 다음과 같습니다:

```
00:01 +1: 모든 테스트 통과!
```

### 서드파티 라이브러리 사용하기: 모킹을 위한 Mockito

복잡한 의존성을 가진 코드를 테스트할 때는 Mockito를 사용하여 모의 객체를 생성할 수 있습니다. 먼저 `pubspec.yaml`에 Mockito를 추가합니다:

```yaml
dev_dependencies:
  mockito: ^5.0.0
```

사용자 데이터를 가져오는 `UserRepository` 클래스가 있고 실제 데이터베이스에 접근하지 않고 `UserRepository`에 의존하는 `UserService`를 테스트하려고 한다고 가정합니다:

```dart
import 'package:mockito/mockito.dart';
import 'package:test/test.dart';
import 'package:your_project/user_repository.dart';
import 'package:your_project/user_service.dart';

// Mockito를 사용하여 Mock 클래스 생성
class MockUserRepository extends Mock implements UserRepository {}

void main() {
  group('UserService 테스트', () {
    test('사용자를 성공적으로 가져옴', () {
      // 모의 인스턴스 생성
      final mockUserRepository = MockUserRepository();
      final userService = UserService(mockUserRepository);

      // 모의 행동 설정
      when(mockUserRepository.fetchUser(1)).thenReturn(User(id: 1, name: '테스트 사용자'));

      // 모의 메소드가 예상된 인자로 호출되었는지 단언
      expect(userService.getUserName(1), '테스트 사용자');
      verify(mockUserRepository.fetchUser(1)).called(1);
    });
  });
}
```

이 테스트를 실행하면 `UserService`가 `UserRepository`와 올바르게 상호 작용하며, 모킹을 사용하여 실제 상호 작용을 제어된 방식으로 시뮬레이션하는 것을 확인할 수 있습니다.
