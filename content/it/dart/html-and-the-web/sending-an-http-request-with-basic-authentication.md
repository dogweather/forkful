---
title:                "Inviare una richiesta HTTP con autenticazione di base"
date:                  2024-03-08T21:56:19.472860-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## Cosa & Perché?

Inviare una richiesta HTTP con autenticazione di base comporta l'aggiunta di un nome utente e di una password a una richiesta per verificare l'identità dell'utente. Gli sviluppatori lo utilizzano per accedere a risorse che richiedono autenticazione, assicurando una comunicazione sicura tra il client e il server.

## Come fare:

In Dart, puoi utilizzare il pacchetto `http` per inviare richieste HTTP con autenticazione di base. Prima di tutto, aggiungi il pacchetto `http` al tuo file `pubspec.yaml`:

```yaml
dependencies:
  http: ^0.13.4
```

Successivamente, importa il pacchetto nel tuo file Dart:

```dart
import 'package:http/http.dart' as http;
import 'dart:convert';
```

Per inviare una richiesta GET con autenticazione di base, puoi utilizzare il seguente codice:

```dart
Future<void> fetchUserData() async {
  final username = 'yourUsername';
  final password = 'yourPassword';
  final credentials = base64Encode(utf8.encode('$username:$password'));
  final response = await http.get(
    Uri.parse('https://yourapi.com/userdata'),
    headers: {
      'Authorization': 'Basic $credentials',
    },
  );

  if (response.statusCode == 200) {
    print('Dati utente recuperati con successo!');
    print('Corpo della risposta: ${response.body}');
  } else {
    print('Impossibile recuperare i dati utente con codice di stato: ${response.statusCode}');
  }
}
```

Questo codice invia una richiesta GET a 'https://yourapi.com/userdata' con un'intestazione di autenticazione di base. Il nome utente e la password vengono codificati in base64 e passati nell'intestazione 'Authorization' secondo gli standard di autenticazione di accesso base.

**Esempio di output:**

In caso di richiesta riuscita e se il server restituisce un codice di stato 200, potresti vedere:

```plaintext
Dati utente recuperati con successo!
Corpo della risposta: {"id":1, "nome":"John Doe", "email":"john@example.com"}
```

Se l'autenticazione fallisce o si verifica un altro errore, il codice di stato della risposta aiuterà a identificare il problema.
