//bondarenko kristina CS-06
#include <iostream>
#include <vector>

using namespace std;

void printMessage(bool x){
    static int counter;
    counter++;
    if (x){
        ::printf("step #%d: permutation\n", counter);
    } else {
        ::printf("step #%d: elimination\n", counter);
    }
}

class Matrix  {

protected:
    std::vector<std::vector<double>> matrixArray;
    int numberOfRows;
    int numberOfColumns;
public:
    Matrix(int rows, int cols) : numberOfRows(rows), numberOfColumns(cols), matrixArray(rows, std::vector<double>(cols)) {}

    virtual double get(int row, int col) const {
        return matrixArray[row][col];
    }

    void put(int row, int col, double value) {
        if (abs(value) < 0.0000000001){
            value = 0.00;
        }
        matrixArray[row][col] = value;
    }

    int getN() const {
        return numberOfRows;
    }

    int getM() const {
        return numberOfColumns;
    }

    friend istream& operator>>(istream& in, Matrix& mat) {
        int n = mat.getN(), m = mat.getM();
        for (int i = 0; i < n; ++i) {
            for (int j = 0; j < m; ++j) {
                in >> mat.matrixArray[i][j];
            }
        }
        return in;
    }

    void operator=(const Matrix& otherMatrix) {
        numberOfRows = otherMatrix.numberOfRows;
        numberOfColumns = otherMatrix.numberOfColumns;
        matrixArray = otherMatrix.matrixArray;
    }

    Matrix operator+(const Matrix& other){
        Matrix result(numberOfRows, numberOfColumns);


        for (int i = 0; i < numberOfRows; i++) {
            for (int j = 0; j < numberOfColumns; j++) {
                double sum = matrixArray[i][j] + other.matrixArray[i][j];
                result.put(i, j, sum);
            }
        }
        return result;
    }

    Matrix operator-(const Matrix& other)  {
        Matrix result(numberOfRows, numberOfColumns);

        for (int i = 0; i < numberOfRows; i++) {
            for (int j = 0; j < numberOfColumns; j++) {
                double difference = matrixArray[i][j] - other.matrixArray[i][j];
                result.put(i, j,difference);
            }
        }

        return result;
    }

    Matrix operator*(const Matrix& other)  {
        Matrix result(numberOfRows, other.numberOfColumns);

        for (int i = 0; i < numberOfRows; i++) {
            for (int j = 0; j < other.numberOfColumns; j++) {
                double res = 0;
                for (int k = 0; k < numberOfColumns; k++) {
                    res += matrixArray[i][k] * other.matrixArray[k][j];
                }
                result.put(i, j, res);
            }
        }

        return result;
    }

    virtual Matrix transpose()  {
        Matrix result(numberOfColumns, numberOfRows);

        for (int i = 0; i < numberOfRows; i++) {
            for (int j = 0; j < numberOfColumns; j++) {
                result.put(j, i, matrixArray[i][j]);
            }
        }

        return result;
    }

    void permutation(int row1, int row2){
        row2--;
        row1--;
        std::vector<double> temp = matrixArray[row1];
        matrixArray[row1] = matrixArray[row2];
        matrixArray[row2] = temp;
    }

    void print(){
        for (int i = 0; i < numberOfRows; ++i) {
            for (int j = 0; j < numberOfColumns; ++j) {
                printf("%.4f ", this->get(i, j));
            }
            cout << "\n";
        }
    }

    void printSep(int initialSize){
        for (int i = 0; i < initialSize; ++i) {
            for (int j = 0; j < initialSize; ++j) {
                printf("%.4f ", this->get(i, j));
            }
            cout << "\n";
        }
        cout << "\n";

        for (int i = 0; i < initialSize; ++i) {
            for (int j = initialSize; j < getM(); ++j) {
                printf("%.4f ", this->get(i, j));
            }
            cout << "\n";
        }
    }

    Matrix elimination(int i, int j){
        i--;
        j--;

        Matrix elimMat(getM(), getN());
        for (int l = 0; l < getN(); l++) {
            for (int k = 0; k < getN(); k++) {
                if (l == k) {
                    elimMat.put(l, k, 1);
                } else {
                    elimMat.put(l, k, 0);
                }
            }
        }

        elimMat.put(i,j,-(this->get(i,j) / this->get(j,j)));

        return elimMat;
    }

    bool isIdentity(int rows, int cols){
        for (int i = 0; i < rows; ++i) {
            for (int j = 0; j < cols; ++j) {
                if (i == j && get(i,j)!=1){
                    return false;
                }
                if (i != j && get(i,j)!= 0){
                    return false;
                }
            }
        }
        return true;
    }

    bool isUpperTriangular(int size){
        for (int i = 0; i < size; ++i) {
            for (int j = 0; j < i; ++j) {
                if (get(i, j) != 0){
                    return false;
                }

            }
        }
        return true;
    }

    bool isLowerTriangular(int size){
        for (int i = 0; i < size; ++i) {
            for (int j = 0; j < i; ++j) {
                if (get(size - i-1, size - j-1) != 0){
                    return false;
                }

            }
        }
        return true;
    }

    void directElim(int initSize, bool united){
        double max_pivot;
        int max_pivot_row, row_counter = 0 ;

        while (!this->isUpperTriangular(initSize) && row_counter < this->getN()){
            row_counter++;
            max_pivot = -1000;
            max_pivot_row = 0;
            for (int i = row_counter-1; i < initSize; ++i) {
                if (abs(this->get(i, row_counter-1)) > max_pivot){
                    max_pivot_row = i;
                    max_pivot = abs(get(i, row_counter-1));
                }
            }

            if (max_pivot_row != row_counter-1){
                this->permutation(row_counter, max_pivot_row+1);
            }


            for (int i = row_counter+1; i <= this->getN(); ++i) {
                if (get(i-1, row_counter-1)!=0){
                    Matrix elimMat = this->elimination(i, row_counter);

                    Matrix temp = elimMat * (*this);
                    for (int i = 0; i < getN(); i++) {
                        for (int j = 0; j < getM(); j++) {
                            put(i, j, temp.get(i, j));
                        }
                    }
                }

            }
        }
    }

    void backElim(int initSize, bool united){
        int row_counter = 0 ;

        while (!this->isLowerTriangular(initSize) && row_counter < this->getN()){
            row_counter++;

            for (int i = row_counter; i < this->getN(); ++i) {
                Matrix elimMat = this->elimination(this->getN()-i, this->getN()- row_counter+1);
                Matrix temp = elimMat * (*this);

                for (int i = 0; i < getN(); i++) {
                    for (int j = 0; j < getM(); j++) {
                        put(i, j, temp.get(i, j));
                    }
                }

            }
        }
    }

    void normalize(int initSize, bool united){

        Matrix normMat(getM(), getN());
        for (int l = 0; l < getM(); l++) {
            for (int k = 0; k < getN(); k++) {
                if (l == k) {
                    normMat.put(l, k, 1);
                } else {
                    normMat.put(l, k, 0);
                }
            }
        }

        for (int i = 0; i < initSize; ++i) {
            if (get(i,i)!=1){
                normMat.put(i,i, 1/get(i,i));
            }
        }

        Matrix temp = normMat * (*this);
        for (int i = 0; i < getN(); i++) {
            for (int j = 0; j < getM(); j++) {
                put(i, j, temp.get(i, j));
            }
        }

    }

    Matrix secondMatrix(int initialSize){
        Matrix matrix(getN(),getM()-initialSize);
        for (int i = 0; i < initialSize; ++i) {
            for (int j = initialSize; j < getM(); ++j) {
                matrix.put(i,j-initialSize,get(i, j));
            }
        }
        return matrix;
    }

    Matrix uniteMatricies(Matrix& matrix){
        Matrix newMatrix(getN(), getM()+matrix.getM());
        for (int i = 0; i < getN(); ++i) {
            for (int j = 0; j < getM(); ++j) {
                newMatrix.put(i, j, get(i, j));
            }

            for (int j = 0; j < matrix.getM(); ++j) {
                newMatrix.put(i,getM()+j,matrix.get(i,j));
            }
        }
        return newMatrix;
    }

};

class SquareMatrix : public Matrix{
public:
    SquareMatrix(int size) : Matrix(size, size) {}

    void operator=(const SquareMatrix& otherMatrix) {
        numberOfRows = otherMatrix.numberOfRows;
        numberOfColumns = otherMatrix.numberOfColumns;
        matrixArray = otherMatrix.matrixArray;
    }

    SquareMatrix operator+(SquareMatrix& other) {
        SquareMatrix result(getN());

        const SquareMatrix* squareOther = dynamic_cast<const SquareMatrix*>(&other);

        if (squareOther != nullptr) {
            for (int i = 0; i < getN(); i++) {
                for (int j = 0; j < getN(); j++) {
                    double sum = get(i, j) + squareOther->get(i, j);
                    result.put(i, j, sum);
                }
            }
        }

        return result;
    }

    SquareMatrix operator-(SquareMatrix& other)  {
        SquareMatrix result(getN());

        const SquareMatrix* squareOther = dynamic_cast<const SquareMatrix*>(&other);

        if (squareOther != nullptr) {
            for (int i = 0; i < getN(); i++) {
                for (int j = 0; j < getN(); j++) {
                    double difference = get(i, j) - squareOther->get(i, j);
                    result.put(i, j, difference);
                }
            }
        }
        return result;
    }

    SquareMatrix operator*(SquareMatrix& other) {
        SquareMatrix result(getN());

        const SquareMatrix* squareOther = dynamic_cast<const SquareMatrix*>(&other);

        if (squareOther != nullptr) {
            for (int i = 0; i < getN(); i++) {
                for (int j = 0; j < getN(); j++) {
                    double res = 0;
                    for (int k = 0; k < getN(); k++) {
                        res += get(i, k) * squareOther->get(k, j);
                    }
                    result.put(i, j, res);
                }
            }
        }
        return result;
    }

    Matrix transpose() override {
        SquareMatrix& derivedObj = static_cast<SquareMatrix&>(*this);
        Matrix result = derivedObj.transposed();

        return static_cast<Matrix>(result);
    }

    SquareMatrix transposed() {
        SquareMatrix result(getN());

        for (int i = 0; i < getN(); i++) {
            for (int j = 0; j < getN(); j++) {
                result.put(j, i, get(i,j));
            }
        }

        return result;
    }

    bool isUpperTriangular(){
        for (int i = 0; i < this->getN(); ++i) {
            for (int j = 0; j < i; ++j) {
                if (get(i, j) != 0){
                    return false;
                }

            }
        }
        return true;
    }

    bool isIdentity(){
        for (int i = 0; i < getN(); ++i) {
            for (int j = 0; j < getN(); ++j) {
                if (i == j && get(i,j)!=1){
                    return false;
                }
                if (i != j && get(i,j)!= 0){
                    return false;
                }
            }
        }
        return true;
    }

    SquareMatrix elimination(int i, int j) {
        i--;
        j--;
        SquareMatrix elimMat(getN());
        for (int l = 0; l < getN(); l++) {
            for (int k = 0; k < getN(); k++) {
                if (l == k) {
                    elimMat.put(l, k, 1);
                } else {
                    elimMat.put(l, k, 0);
                }
            }
        }
        elimMat.put(i,j,-(this->get(i,j) / this->get(j,j)));
        return elimMat;
    }

    double determinant(){
        double max_pivot;
        int max_pivot_row, row_counter = 0 ;
        double result = 1.00;

        while (!this->isUpperTriangular() && row_counter < this->getN()){
            row_counter++;
            max_pivot = -1000;
            max_pivot_row = 0;
            for (int i = row_counter-1; i < this->getN(); ++i) {
                if (abs(this->get(i, row_counter-1)) > max_pivot){
                    max_pivot_row = i;
                    max_pivot = abs(get(i, row_counter-1));
                }
            }


            if (max_pivot_row != row_counter-1){
                this->permutation(row_counter, max_pivot_row+1);
            }

            for (int i = row_counter+1; i <= this->getN(); ++i) {
                SquareMatrix elimMat = this->elimination(i, row_counter);

                SquareMatrix temp = elimMat * (*this);
                for (int i = 0; i < getN(); i++) {
                    for (int j = 0; j < getN(); j++) {
                        put(i, j, temp.get(i, j));
                    }
                }
                print();
            }
        }

        for (int i = 0; i < this->getN(); ++i) {
            result *= this->get(i, i);
        }
        return result;
    }

    Matrix augmentedMatrix(){
        Matrix newMatrix(getN(), getN()*2);
        for (int i = 0; i < getN(); ++i) {
            for (int j = 0; j < getN(); ++j) {
                newMatrix.put(i, j, get(i, j));
            }
            for (int j = 0; j < getN(); ++j) {
                if (i != j){
                    newMatrix.put(i, getN()+j, 0);
                } else {
                    newMatrix.put(i, getN()+j, 1);
                }
            }
        }
        return newMatrix;
    }

    Matrix uniteMatricies(Matrix& matrix){
        Matrix newMatrix(getN(), getN()+matrix.getM());
        for (int i = 0; i < getN(); ++i) {
            for (int j = 0; j < getN(); ++j) {
                newMatrix.put(i, j, get(i, j));
            }
            for (int j = 0; j < matrix.getM(); ++j) {
                newMatrix.put(i,getN()+j,matrix.get(i,j));
            }
        }
        return newMatrix;
    }

    void backFromAugmentedMatrix(Matrix& matrix){
        for (int i = 0; i < matrix.getN(); ++i) {
            for (int j = 0; j < matrix.getN(); ++j) {
                put(i,j,matrix.get(i, matrix.getN()+j));
            }
        }
    }

};

class IdentityMatrix : public SquareMatrix {

public:
    IdentityMatrix(int size) : SquareMatrix(size) {
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++) {
                if (i==j){
                    put(i,j,1);
                } else{
                    put(i,j,0);
                }
            }
        }
    }


};

class ColumnVector: public Matrix{
public:
    ColumnVector(int size) : Matrix(size, 1) {}

    double get(int i) const {
        return Matrix::get(i, 0);
    }

    void put(int i, double value) {
        Matrix::put(i, 0, value);
    }

    ColumnVector operator+(const ColumnVector& other) const {
        ColumnVector result(getN());
        for (int i = 0; i < getN(); i++) {
            result.put(i, (*this).get(i) + other.get(i));
        }
        return result;
    }

    ColumnVector operator*(double scalar) {
        ColumnVector result(getN());
        for (int i = 0; i < getN(); i++) {
            result.put(i, get(i) * scalar);
        }
        return result;
    }


    Matrix uniteVectors(ColumnVector& vector){
        Matrix newMatrix(getN(), 2);
        for (int i = 0; i < getN(); ++i) {
            newMatrix.put(i, 0, get(i));
            newMatrix.put(i,1,vector.get(i));

        }
        return newMatrix;
    }
};

int main() {

    int m, n, new_value_a, new_value_b;
    cin >> m;
    ColumnVector A(m), b(m);
    for (int i = 0; i < m; ++i) {
        cin >> new_value_a;
        A.put(i, new_value_a);

        cin >> new_value_b;
        b.put(i, new_value_b);
    }
    cin >> n;

    if (n == 1){
        Matrix matrixA(m, 2);
        ColumnVector vector1(m);
        for (int i = 0; i < m; ++i) {
            vector1.put(i,1);
        }
        matrixA = vector1.uniteVectors(A);
        cout << "A:\n";
        matrixA.print();
        cout << "A_T*A:\n";
        SquareMatrix matrixATA(2);
        Matrix temp(2,2);
        temp = matrixA.transpose() * matrixA;
        for (int i = 0; i < 2; ++i) {
            for (int j = 0; j < 2; ++j) {
                matrixATA.put(i, j, temp.get(i,j));
            }
        }
        matrixATA.print();
        cout << "(A_T*A)^-1:\n";
        Matrix augmentMatrix = matrixATA.augmentedMatrix();

        augmentMatrix.directElim(matrixATA.getN(), true);

        augmentMatrix.backElim(matrixATA.getN(), true);

        augmentMatrix.normalize(matrixATA.getN(), true);
        SquareMatrix result(matrixATA.getN());
        result.backFromAugmentedMatrix(augmentMatrix);
        result.print();

        cout << "A_T*b:\n";
        Matrix vectorAtb(m, 1);
        vectorAtb = matrixA.transpose() * b;
        vectorAtb.print();

        cout << "x~:\n";
        Matrix x(m, 1);
        x = (Matrix)result * vectorAtb;
        x.print();
    }
    if (n == 2){
        Matrix matrixA(m, 3);
        Matrix temp0(m,3);
        ColumnVector vector1(m);
        ColumnVector vector2(m);


        for (int i = 0; i < m; ++i) {
            vector1.put(i,1);
            vector2.put(i, A.get(i)*A.get(i));
        }

        temp0 = vector1.uniteVectors(A);

        matrixA = temp0.uniteMatricies(vector2);
        cout << "A:\n";
        matrixA.print();
        cout << "A_T*A:\n";
        SquareMatrix matrixATA(3);
        Matrix temp(3,3);
        temp = matrixA.transpose() * matrixA;
        for (int i = 0; i < 3; ++i) {
            for (int j = 0; j < 3; ++j) {
                matrixATA.put(i, j, temp.get(i,j));
            }
        }
        matrixATA.print();


        cout << "(A_T*A)^-1:\n";
        Matrix augmentMatrix = matrixATA.augmentedMatrix();

        augmentMatrix.directElim(matrixATA.getN(), true);

        augmentMatrix.backElim(matrixATA.getN(), true);

        augmentMatrix.normalize(matrixATA.getN(), true);
        SquareMatrix result(matrixATA.getN());
        result.backFromAugmentedMatrix(augmentMatrix);
        result.print();

        cout << "A_T*b:\n";
        Matrix vectorAtb(m, 1);
        vectorAtb = matrixA.transpose() * b;
        vectorAtb.print();

        cout << "x~:\n";
        Matrix x(m, 1);
        x = (Matrix)result * vectorAtb;
        x.print();
    }
    return 0;
}


