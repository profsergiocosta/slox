# 📚 slox — A Tree-Walk Interpreter em Scala 3

Este projeto é uma implementação do interpretador **Lox**, inspirado no livro *Crafting Interpreters* de Bob Nystrom.  
A principal diferença é que toda a estrutura foi adaptada para **Scala 3**, usando **Algebraic Data Types (ADTs)** no lugar do padrão Visitor.

---

## ✨ **Principais características**

- Interpretador **Tree-Walk** escrito em Scala 3
- AST representada com **`sealed trait` + `case class`** (ADT) em vez de Visitor
- Controle de fluxo `return` implementado com exceção (`Return`) idiomática
- Variáveis globais e escopo com **`Environment`**
- Funções definidas pelo usuário (sem closures ainda)
- Suporte a `if`, `while`, `for`, `and`, `or`
- Tratamento de erros em tempo de execução (`RuntimeError`)

---

## ✅ **Progresso com base no Crafting Interpreters**

### 📖 **Capítulos concluídos**

- [x] **A Tree-Walk Interpreter**
- [x] **4. Scanning**
  - [x] *Design Note: Implicit Semicolons*
- [x] **5. Representing Code**
- [x] **6. Parsing Expressions**
  - [x] *Design Note: Logic Versus History*
- [x] **7. Evaluating Expressions**
  - [x] *Design Note: Static and Dynamic Typing*
- [x] **8. Statements and State**
  - [x] *Design Note: Implicit Variable Declaration*
- [x] **9. Control Flow**
  - [x] *Design Note: Spoonfuls of Syntactic Sugar*
- [x] **10. Functions**
  - [ ] *Closures ainda não implementados*

### ⏳ **Próximos capítulos**

- [ ] **11. Resolving and Binding**
- [ ] **12. Classes**
  - [ ] *Design Note: Prototypes and Power*
- [ ] **13. Inheritance**

---

## 🚀 **Como executar**

```bash
# Compile e execute com Scala CLI ou sbt
sbt run
