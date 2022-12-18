### Groupify

* Groupify is a programming language which determines if a provided set and binary operation form a group.

* The group axioms that the set and binary operation must satisfy are
  
  1. Closure
  2. (Unique) Identity
  3. (Unique) Inverses
  4. Associativity
  
* Given a valid set and binary operation, Groupify returns ```true``` if the set forms a group under the binary operation, and ```false``` otherwise.

* Usage:
 
 ```
 cd code/backend/Groupify
 dotnet build
 dotnet run "{0,1,2,3,4} +%5"
 dotnet run ../GroupifyTest/examples/example-<example number>.groupify
 ```

View a presentation of Groupify here: https://drive.google.com/file/d/19ThJkqR28M6G0loOaMZ0rY8wxzEDNPpM/view?usp=share_link
